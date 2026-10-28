//! Branch on opaque predicates; only rustc decides which branch is active.
use super::*;
use syn::parse::Parser;

// The first top-level comma separates the predicate from cfg_attr's payload.
// Parenthesized predicates and attribute arguments remain opaque token groups.
fn parts(attr: &syn::Attribute) -> syn::Result<Option<(TokenStream2, Option<TokenStream2>)>> {
    if !attr.path().is_ident("cfg") && !attr.path().is_ident("cfg_attr") {
        return Ok(None);
    }
    let list = attr.meta.require_list()?;
    if attr.path().is_ident("cfg") {
        return Ok(Some((list.tokens.clone(), None)));
    }
    let mut tokens = list.tokens.clone().into_iter();
    let mut predicate = TokenStream2::new();
    for token in tokens.by_ref() {
        if matches!(&token, proc_macro2::TokenTree::Punct(p) if p.as_char() == ',') {
            return Ok(Some((predicate, Some(tokens.collect()))));
        }
        predicate.extend([token]);
    }
    Err(syn::Error::new_spanned(
        attr,
        "expected cfg_attr(predicate, attributes...)",
    ))
}

#[derive(Default)]
struct Filter<'a> {
    assignment: Option<(&'a str, bool)>,
    first: Option<TokenStream2>,
    error: Option<syn::Error>,
}

impl Filter<'_> {
    fn attrs(&mut self, attrs: &mut Vec<syn::Attribute>) -> bool {
        let mut retained = Vec::new();
        let mut active = true;
        for attr in core::mem::take(attrs) {
            let parts = match parts(&attr) {
                Ok(parts) => parts,
                Err(error) => {
                    self.error.get_or_insert(error);
                    retained.push(attr);
                    continue;
                }
            };
            let Some((predicate, payload)) = parts else {
                retained.push(attr);
                continue;
            };
            self.first.get_or_insert_with(|| predicate.clone());
            let Some((key, enabled)) = self.assignment else {
                retained.push(attr);
                continue;
            };
            if predicate.to_string() != key {
                retained.push(attr);
                continue;
            }
            match payload {
                None => active &= enabled,
                Some(payload) if enabled => {
                    // Parse attribute envelopes, never predicate expressions or
                    // the token contents of list-style attribute arguments.
                    match Punctuated::<syn::Meta, Token![,]>::parse_terminated.parse2(payload) {
                        Ok(payload) => {
                            retained.extend(payload.into_iter().map(|meta| parse_quote!(#[#meta])))
                        }
                        Err(error) => {
                            self.error.get_or_insert(error);
                        }
                    }
                }
                Some(_) => {}
            }
        }
        *attrs = retained;
        active
    }

    fn input(&mut self, input: &mut TopLevelImpls) {
        if let Some(trait_) = &mut input.trait_ {
            if self.attrs(&mut trait_.attrs) {
                self.visit_item_trait_mut(trait_);
            } else {
                input.trait_ = None;
            }
        }
        input.impls.retain_mut(|item| {
            if !self.attrs(&mut item.attrs) {
                return false;
            }
            self.visit_item_impl_mut(item);
            true
        });
    }
}

impl VisitMut for Filter<'_> {
    fn visit_item_trait_mut(&mut self, item: &mut ItemTrait) {
        item.items.retain_mut(|item| {
            let attrs = match item {
                syn::TraitItem::Const(item) => &mut item.attrs,
                syn::TraitItem::Type(item) => &mut item.attrs,
                syn::TraitItem::Fn(item) => &mut item.attrs,
                syn::TraitItem::Macro(item) => &mut item.attrs,
                _ => return true,
            };
            self.attrs(attrs)
        });
        syn::visit_mut::visit_item_trait_mut(self, item);
    }

    fn visit_item_impl_mut(&mut self, item: &mut ItemImpl) {
        item.items.retain_mut(|item| {
            let attrs = match item {
                syn::ImplItem::Const(item) => &mut item.attrs,
                syn::ImplItem::Type(item) => &mut item.attrs,
                syn::ImplItem::Fn(item) => &mut item.attrs,
                syn::ImplItem::Macro(item) => &mut item.attrs,
                _ => return true,
            };
            self.attrs(attrs)
        });
        syn::visit_mut::visit_item_impl_mut(self, item);
    }

    fn visit_generics_mut(&mut self, generics: &mut syn::Generics) {
        generics.params = core::mem::take(&mut generics.params)
            .into_iter()
            .filter_map(|mut param| {
                let attrs = match &mut param {
                    syn::GenericParam::Lifetime(param) => &mut param.attrs,
                    syn::GenericParam::Type(param) => &mut param.attrs,
                    syn::GenericParam::Const(param) => &mut param.attrs,
                };
                self.attrs(attrs).then_some(param)
            })
            .collect();
        syn::visit_mut::visit_generics_mut(self, generics);
    }

    fn visit_signature_mut(&mut self, sig: &mut syn::Signature) {
        sig.inputs = core::mem::take(&mut sig.inputs)
            .into_iter()
            .filter_map(|mut arg| {
                let attrs = match &mut arg {
                    syn::FnArg::Receiver(arg) => &mut arg.attrs,
                    syn::FnArg::Typed(arg) => &mut arg.attrs,
                };
                self.attrs(attrs).then_some(arg)
            })
            .collect();
        syn::visit_mut::visit_signature_mut(self, sig);
    }

    // Rust processes implementation expressions and macro token bodies. They do
    // not determine the dispatch schema and must not multiply its variants.
    fn visit_expr_mut(&mut self, _: &mut syn::Expr) {}
    fn visit_block_mut(&mut self, _: &mut syn::Block) {}
}

fn first_predicate(input: &TopLevelImpls) -> syn::Result<Option<TokenStream2>> {
    let mut finder = Filter::default();
    finder.input(&mut input.clone());
    if let Some(error) = finder.error {
        return Err(error);
    }
    Ok(finder.first)
}

/// Fully expand both values of one opaque predicate, then factor their output.
/// This is not a successful-expansion heuristic: errors are output too, under
/// the corresponding guard. In either configuration, rustc sees exactly the
/// declarations (or diagnostics) that ordinary branching would have produced.
fn expand_single_condition(input: &TopLevelImpls) -> syn::Result<Option<TokenStream2>> {
    let Some(predicate) = first_predicate(input)? else {
        return Ok(None);
    };
    let key = predicate.to_string();
    let mut variants = Vec::new();
    for enabled in [true, false] {
        let mut variant = input.clone();
        loop {
            let mut filter = Filter {
                assignment: Some((&key, enabled)),
                ..Default::default()
            };
            filter.input(&mut variant);
            if let Some(error) = filter.error {
                variants.push(Err(error));
                break;
            }
            match first_predicate(&variant) {
                Ok(None) => {
                    variants.push(Ok(variant));
                    break;
                }
                Ok(Some(next)) if next.to_string() == key => {}
                Ok(Some(_)) => return Ok(None),
                Err(error) => {
                    variants.push(Err(error));
                    break;
                }
            }
        }
    }
    let mut outputs = variants
        .into_iter()
        .map(|variant| {
            let result = variant.and_then(|TopLevelImpls { trait_, impls }| {
                let grouped = syn::parse2(quote!(#trait_ #(#impls)*))?;
                generate_expansion(grouped)
            });
            match result {
                Ok((trait_, body)) => {
                    // generate_expansion always emits this anonymous item scope.
                    let scope: syn::ItemConst = syn::parse2(body)?;
                    let syn::Expr::Block(block) = *scope.expr else {
                        unreachable!()
                    };
                    Ok((
                        trait_
                            .into_iter()
                            .map(|item| quote!(#item))
                            .collect::<Vec<_>>(),
                        block
                            .block
                            .stmts
                            .into_iter()
                            .map(|stmt| quote!(#stmt))
                            .collect::<Vec<_>>(),
                    ))
                }
                Err(error) => {
                    let error = error.into_compile_error();
                    Ok((vec![quote!(const _: () = { #error };)], Vec::new()))
                }
            }
        })
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter();
    let (enabled_root, enabled_body) = outputs.next().unwrap();
    let (disabled_root, disabled_body) = outputs.next().unwrap();
    let root = factor_declarations(enabled_root, disabled_root, &predicate);
    let body = factor_declarations(enabled_body, disabled_body, &predicate);
    Ok(Some(quote! {
        #root
        #[allow(clippy::needless_lifetimes)]
        const _: () = { #body };
    }))
}

fn factor_declarations(
    enabled: Vec<TokenStream2>,
    disabled: Vec<TokenStream2>,
    predicate: &TokenStream2,
) -> TokenStream2 {
    // Include token span/debug information rather than merging declarations
    // solely by their printed spelling. Preserve multiplicity within a branch.
    let mut disabled = disabled
        .into_iter()
        .map(|tokens| (format!("{tokens:?}"), Some(tokens)))
        .collect::<Vec<_>>();
    let mut output = TokenStream2::new();
    for tokens in enabled {
        let key = format!("{tokens:?}");
        if let Some((_, other)) = disabled
            .iter_mut()
            .find(|(k, value)| *k == key && value.is_some())
        {
            other.take();
            output.extend(tokens);
        } else {
            output.extend(quote!(#[cfg(#predicate)] #tokens));
        }
    }
    for (_, tokens) in disabled {
        if let Some(tokens) = tokens {
            output.extend(quote!(#[cfg(not(#predicate))] #tokens));
        }
    }
    output
}

pub(super) fn expand(input: &TopLevelImpls) -> syn::Result<Option<TokenStream2>> {
    if first_predicate(input)?.is_none() {
        return Ok(None);
    }
    if let Some(expansion) = expand_single_condition(input)? {
        return Ok(Some(expansion));
    }
    let name = match proc_macro_crate::crate_name("disjoint_impls") {
        Ok(proc_macro_crate::FoundCrate::Name(name)) => name,
        // A proc macro cannot be invoked within its own library. Its package's
        // integration tests and binaries refer to it as an external crate too.
        Ok(proc_macro_crate::FoundCrate::Itself) => "disjoint_impls".into(),
        Err(error) => return Err(syn::Error::new(proc_macro2::Span::call_site(), error)),
    };
    let name = format_ident!("{name}");
    expand_with_path(input, quote!(::#name::disjoint_impls)).map(Some)
}

fn expand_with_path(input: &TopLevelImpls, path: TokenStream2) -> syn::Result<TokenStream2> {
    let mut pending = vec![(
        input.clone(),
        IndexMap::<String, (TokenStream2, bool)>::new(),
    )];
    let mut output = TokenStream2::new();
    while let Some((input, assignments)) = pending.pop() {
        let Some(predicate) = first_predicate(&input)? else {
            let guards = assignments.values().map(|(predicate, enabled)| {
                if *enabled {
                    quote!(#[cfg(#predicate)])
                } else {
                    quote!(#[cfg(not(#predicate))])
                }
            });
            let TopLevelImpls { trait_, impls } = input;
            output.extend(quote! {
                #(#guards)*
                #path! { #trait_ #(#impls)* }
            });
            continue;
        };
        let key = predicate.to_string();
        let choices = assignments
            .get(&key)
            .map(|(_, enabled)| vec![*enabled])
            .unwrap_or_else(|| vec![false, true]);
        for enabled in choices {
            let mut variant = input.clone();
            let mut filter = Filter {
                assignment: Some((&key, enabled)),
                ..Default::default()
            };
            filter.input(&mut variant);
            if let Some(error) = filter.error {
                return Err(error);
            }
            let mut assignments = assignments.clone();
            assignments.insert(key.clone(), (predicate.clone(), enabled));
            pending.push((variant, assignments));
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_condition_gates_errors_and_handles_signature_changes() {
        for tokens in [
            quote! {
                trait Trait { fn required(); }
                #[cfg(P)] impl Trait for u8 {}
            },
            quote! {
                trait Trait { fn f(#[cfg(P)] x: u8); }
                impl Trait for u8 { fn f(#[cfg(P)] x: u8) {} }
            },
            quote! {
                trait Trait { #[cfg_attr(P, cfg(P))] fn f(); }
                impl Trait for u8 { #[cfg_attr(P, cfg(P))] fn f() {} }
            },
            quote! {
                #[cfg(P)] impl<T> Container<T> { fn f() {} }
                impl Container<u8> { fn g() {} }
            },
        ] {
            let input = syn::parse2(tokens).unwrap();
            let expansion = expand_single_condition(&input).unwrap().unwrap();
            assert!(!expansion.to_string().contains("disjoint_impls !"));
            syn::parse2::<syn::File>(expansion).unwrap();
        }
        let input = syn::parse2(quote! {
            trait Trait {}
            #[cfg_attr(P, cfg(Q))] impl Trait for u8 {}
        })
        .unwrap();
        assert!(expand_single_condition(&input).unwrap().is_none());
    }

    #[test]
    fn conditional_members_expand_without_macro_calls() {
        let input = syn::parse2(quote! {
            trait Trait {
                #[cfg(P)] type Data;
                #[cfg(P)] const ENABLED: bool;
                #[cfg(P)] fn value(&self);
            }
            impl<T: Dispatch<Group = A>> Trait for T {
                #[cfg(P)] type Data = FeatureOnly;
                #[cfg(P)] const ENABLED: bool = true;
                #[cfg(P)] fn value(&self) {}
            }
            impl<T: Dispatch<Group = B>> Trait for T {
                #[cfg(P)] type Data = FeatureOnly;
                #[cfg(P)] const ENABLED: bool = true;
                #[cfg(P)] fn value(&self) {}
            }
        })
        .unwrap();
        let expansion = expand_single_condition(&input)
            .unwrap()
            .unwrap()
            .to_string();
        assert!(!expansion.contains("disjoint_impls !"));
        assert!(!expansion.contains("compile_error"));
        for tokens in [
            quote! { trait Trait { #[cfg(P)] fn f(); } impl Trait for u8 { fn f() {} } },
            quote! { trait Trait { #[cfg_attr(P, inline)] fn f(); } impl Trait for u8 { fn f() {} } },
            quote! { trait Trait { #[cfg(P)] fn f<T>(); } impl Trait for u8 { #[cfg(P)] fn f<T>() {} } },
        ] {
            let input = syn::parse2(tokens).unwrap();
            assert!(expand_single_condition(&input).unwrap().is_some());
        }
    }

    #[test]
    fn singleton_group_keeps_its_gate_without_macro_branches() {
        let input = syn::parse2(quote! {
            trait Trait {}
            impl Trait for u8 {}
            #[cfg(opaque => tokens)] impl Trait for FeatureType {}
        })
        .unwrap();
        let expansion = expand_single_condition(&input).unwrap().unwrap();
        let file: syn::File = syn::parse2(expansion.clone()).unwrap();
        assert_eq!(file.items.len(), 2);
        assert!(expansion.to_string().contains("cfg (opaque => tokens)"));
        assert!(!expansion.to_string().contains("not ("));
        assert!(!expansion.to_string().contains("disjoint_impls !"));
    }

    #[test]
    fn shared_dispatch_remains_unconditional() {
        let input = syn::parse2(quote! {
            trait Trait {}
            impl<T: Dispatch<Group = A>> Trait for T {}
            #[cfg(feature = "extra")]
            impl<T: Dispatch<Group = B>> Trait for T {}
        })
        .unwrap();
        let expansion = expand_single_condition(&input).unwrap().unwrap();
        let file: syn::File = syn::parse2(expansion).unwrap();
        let syn::Item::Const(scope) = &file.items[1] else {
            panic!()
        };
        let syn::Expr::Block(block) = &*scope.expr else {
            panic!()
        };
        let impls = block
            .block
            .stmts
            .iter()
            .filter_map(|stmt| match stmt {
                syn::Stmt::Item(syn::Item::Impl(item)) => Some(item),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(impls.len(), 3);
        assert_eq!(
            impls
                .iter()
                .filter(|item| item.attrs.iter().any(|attr| attr.path().is_ident("cfg")))
                .count(),
            1
        );
        let public = impls
            .iter()
            .find(|item| item.trait_.as_ref().unwrap().0.is_ident("Trait"))
            .unwrap();
        assert!(!public.attrs.iter().any(|attr| attr.path().is_ident("cfg")));
        assert!(!quote!(#public).to_string().contains(" B"));
    }

    #[test]
    fn additional_predicates_keep_branching() {
        for tokens in [
            quote! {
                trait Trait {}
                #[cfg(P)] impl Trait for u8 {}
                #[cfg(not(P))] impl Trait for u8 {}
            },
            quote! {
                trait Trait {}
                impl<T: Dispatch<Group = A>> Trait for T {}
                #[cfg_attr(P, cfg(Q))] impl<T: Dispatch<Group = B>> Trait for T {}
            },
        ] {
            let input = syn::parse2(tokens).unwrap();
            assert!(expand_single_condition(&input).unwrap().is_none());
        }
    }

    #[test]
    fn predicates_are_opaque_and_equal_tokens_share_a_decision() {
        let input = syn::parse2(quote! {
            trait Trait {}
            #[cfg(arbitrary => tokens)] impl Trait for u8 {}
            #[cfg(arbitrary => tokens)] impl Trait for u16 {}
        })
        .unwrap();
        let output = expand_with_path(&input, quote!(disjoint_impls)).unwrap();
        let file: syn::File = syn::parse2(output).unwrap();
        assert_eq!(file.items.len(), 2);
    }

    #[test]
    fn logically_related_predicates_are_not_interpreted() {
        let input = syn::parse2(quote! {
            trait Trait {}
            #[cfg(feature = "a")] impl Trait for u8 {}
            #[cfg(not(feature = "a"))] impl Trait for u8 {}
        })
        .unwrap();
        let output = expand_with_path(&input, quote!(disjoint_impls)).unwrap();
        let file: syn::File = syn::parse2(output).unwrap();
        // Including the impossible combinations: Rust discards them before
        // invoking the macro, so they never undergo dispatch validation.
        assert_eq!(file.items.len(), 4);
        assert!(
            file.items
                .iter()
                .all(|item| matches!(item, syn::Item::Macro(_)))
        );
    }

    #[test]
    fn nested_cfg_attr_reuses_the_same_opaque_predicate() {
        let input = syn::parse2(quote! {
            trait Trait {}
            #[cfg_attr(P, cfg_attr(P, custom(a => b)))]
            impl Trait for u8 {}
        })
        .unwrap();
        let output = expand_with_path(&input, quote!(disjoint_impls)).unwrap();
        let file: syn::File = syn::parse2(output).unwrap();
        assert_eq!(file.items.len(), 2);
    }

    #[test]
    fn cfg_attr_payload_order_and_duplicates_are_preserved() {
        let input = syn::parse2(quote! {
            trait Trait {}
            #[cfg_attr(opaque => tokens, custom(a => b), custom(a => b))]
            impl Trait for u8 {}
        })
        .unwrap();
        let key = quote!(opaque => tokens).to_string();
        let mut enabled = input;
        Filter {
            assignment: Some((&key, true)),
            ..Default::default()
        }
        .input(&mut enabled);
        let attrs = &enabled.impls[0].attrs;
        assert_eq!(attrs.len(), 2);
        assert_eq!(attrs[0], attrs[1]);
        assert_eq!(
            quote!(#(#attrs)*).to_string(),
            quote!(#[custom(a => b)] #[custom(a => b)]).to_string()
        );
    }
}
