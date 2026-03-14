use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group: ?Sized;
}

enum GroupA {}
enum GroupB {}

impl Dispatch for str {
    type Group = GroupA;
}

disjoint_impls! {
    trait Kita: Sized {}

    impl Kita for str
    where
        Self: for<'a> Sized + Dispatch<Group = GroupA>,
    {}
    impl Kita for str
    where
        for<'b> str: Sized + for<'a> Dispatch<Group = GroupB>,
    {}
}

/*
trait Kita: Sized {}

const _: () = {
    trait Kita0<_TŠČ0: ?Sized>: Kita {}
    impl Kita0<GroupA> for str
    where
        str: for<'a> Sized + Dispatch<Group = GroupA>,
    {}
    impl Kita0<GroupB> for str
    where
        str: for<'b> Sized + for<'a> Dispatch<Group = GroupB>,
    {}
    impl Kita for str
    where
        str: Dispatch,
        str: for<'a> Sized,
        Self: for<'_dšč> Kita0<<str as Dispatch>::Group>,
    {}
};
*/

fn main() {}
