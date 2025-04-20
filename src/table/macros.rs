#[doc(hidden)]
#[macro_export]
macro_rules! opt {
    () => { None };
    ($($something:tt)+) => { Some($($something)+) };
}

#[doc(hidden)]
#[macro_export]
macro_rules! bind {
    ($table:ident, $inc:ident, _) => {
        $inc()
    };
    ($table:ident, $inc:ident, $name:ident) => {
        $inc()
    };
    ($table:ident, $inc:ident, =$token:ident) => {
        $table.bind(stringify!($token))
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! save_bind {
    ($table:ident, $token:expr, $name:ident) => {
        $table.name_bind(&$token, stringify!($name))
    };
    ($table:ident, $token:expr, $($tt:tt)+) => {};
}

#[doc(hidden)]
#[macro_export]
macro_rules! assoc {
    (_) => {
        $crate::Assoc::R
    };
    ($ident:ident) => {
        $crate::Assoc::$ident
    };
}

/// Used to construct the pratt table
///
/// The token type being operated on is expected to be in scope with the name
/// Token
///
/// Format of each row is TokenType => [ prefix, infix, assoc, bind ];
///
/// Allowed assoc values are L (left associative), R/_ (right associative)
///
/// Using _ in the bind column assigns a new fresh binding power, binding power
/// increases going down the table (e.g lower rows bind tighter by default).
/// An identifier can be used in the bind column to mint a new binding power,
/// assign it to the current row and also give it a name. Using =NAME in another
/// row will assign the same binding power to that row
///
/// # Example
#[doc = include_str!("example.rs")]
#[macro_export]
macro_rules! pratt {
    (
        $($token:ident => [ $($prefix:ident)?, $($infix:ident)?, $assoc:tt, $($bind:tt)+ ]);* $(;)?
    ) => { {
        static mut BIND: u8 = 1;
        #[allow(unsafe_code)]
        fn inc() -> u8 {
            let bind = unsafe { BIND };
            unsafe { BIND += 1; }
            bind
        }
        #[allow(unsafe_code)]
        unsafe { BIND = 1; }
        let mut table = $crate::Table::new();
        $(
            table.add_row(
                <Token as $crate::Token>::Type::$token,
                $crate::opt!($($prefix)?),
                $crate::opt!($($infix)?),
                $crate::bind!(table, inc, $($bind)+),
                $crate::assoc!($assoc)
            );
            $crate::save_bind!(
                table,
                <Token as $crate::Token>::Type::$token,
                $($bind)+
            );
        )*
        table
    } }
}
