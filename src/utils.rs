pub mod utils {
    use std::any::TypeId;

    pub fn type_eq<T: 'static, U: 'static>() -> bool {
        TypeId::of::<T>() == TypeId::of::<U>()
    }

    #[test]
    fn type_eq_works() {
        assert!(type_eq::<u8, u8>());
        assert_eq!(type_eq::<u8, u16>(), false);
    }
}
