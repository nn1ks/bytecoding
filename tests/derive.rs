use bytecoding::Bytecode;
use std::fmt;

fn test_encode<T: Bytecode + Eq + fmt::Debug>(instructions: &[T], expected_bytes: &[u8]) {
    let mut bytes = Vec::new();
    for i in instructions {
        i.encode(&mut bytes);
    }
    assert_eq!(&bytes, expected_bytes);
}

fn test_decode<T: Bytecode + Eq + fmt::Debug>(bytes: &[u8], expected_instructions: &[T]) {
    let mut bytes: &[u8] = &bytes;
    let mut instructions = Vec::new();
    while !bytes.is_empty() {
        instructions.push(T::decode(&mut bytes).unwrap());
    }
    assert_eq!(instructions, expected_instructions);
}

fn test_encode_decode<T: Bytecode + Eq + fmt::Debug>(instructions: &[T], expected_bytes: &[u8]) {
    test_encode(instructions, expected_bytes);
    test_decode(expected_bytes, instructions);
}

#[test]
fn instruction_type() {
    #[derive(Debug, PartialEq, Eq, Bytecode)]
    #[bytecode(type = u8)]
    enum InstructionU8 {
        A,
        B,
    }
    test_encode_decode(&[InstructionU8::A, InstructionU8::B], &[0, 1]);

    #[derive(Debug, PartialEq, Eq, Bytecode)]
    #[bytecode(type = u16)]
    enum InstructionU16 {
        A,
        B,
    }
    test_encode_decode(&[InstructionU16::A, InstructionU16::B], &[0, 0, 0, 1]);

    #[derive(Debug, PartialEq, Eq, Bytecode)]
    #[bytecode(type = u32)]
    enum InstructionU32 {
        A,
        B,
    }
    test_encode_decode(
        &[InstructionU32::A, InstructionU32::B],
        &[0, 0, 0, 0, 0, 0, 0, 1],
    );

    #[derive(Debug, PartialEq, Eq, Bytecode)]
    #[bytecode(type = u64)]
    enum InstructionU64 {
        A,
        B,
    }
    test_encode_decode(
        &[InstructionU64::A, InstructionU64::B],
        &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    );
}

#[test]
fn flatten() {
    #[derive(Debug, PartialEq, Eq, Bytecode)]
    #[bytecode(type = u8)]
    enum Instruction {
        Foo(u16, #[bytecode(flatten = [1, 0])] u64),
        Bar {
            #[bytecode(flatten = [10, 11, 12, 13])]
            a: u32,
            b: u8,
        },
        Baz(#[bytecode(flatten = [100])] u8),
    }

    let instructions = [
        Instruction::Foo(10, 1),
        Instruction::Foo(11, 0),
        Instruction::Foo(12, 2),
        Instruction::Bar { a: 10, b: 42 },
        Instruction::Bar { a: 13, b: 42 },
        Instruction::Bar { a: 0, b: 42 },
        Instruction::Bar { a: 14, b: 42 },
        Instruction::Baz(100),
        Instruction::Baz(99),
    ];
    #[rustfmt::skip]
    let expected_bytes = [
        0, 0, 10,
        1, 0, 11,
        2, 0, 12, 0, 0, 0, 0, 0, 0, 0, 2,
        3, 42,
        6, 42,
        7, 0, 0, 0, 0, 42,
        7, 0, 0, 0, 14, 42,
        8,
        9, 99,
    ];
    test_encode_decode(&instructions, &expected_bytes);
}

#[test]
fn flatten_all() {
    #[derive(Debug, PartialEq, Eq)]
    enum Foo {
        Bar,
        Baz,
    }

    #[derive(Debug, PartialEq, Eq, Bytecode)]
    #[bytecode(type = u8)]
    enum Instruction {
        A(#[bytecode(flatten_all = [true, false])] bool),
        B {
            #[bytecode(flatten_all = [true, false])]
            value: bool,
        },
        C(u32, #[bytecode(flatten_all = [false, true])] bool),
        D {
            #[bytecode(flatten_all = [false, true])]
            value1: bool,
            value2: u16,
            value3: u8,
        },
        E(#[bytecode(flatten_all = [Foo::Bar, Foo::Baz])] Foo),
    }

    let instructions = [
        Instruction::A(true),
        Instruction::A(false),
        Instruction::B { value: true },
        Instruction::B { value: false },
        Instruction::C(10, false),
        Instruction::C(11, true),
        Instruction::D {
            value1: false,
            value2: 20,
            value3: 1,
        },
        Instruction::D {
            value1: true,
            value2: 21,
            value3: 2,
        },
        Instruction::E(Foo::Bar),
        Instruction::E(Foo::Baz),
    ];
    #[rustfmt::skip]
    let expected_bytes = [
        0,
        1,
        2,
        3,
        4, 0, 0, 0, 10,
        5, 0, 0, 0, 11,
        6, 0, 20, 1,
        7, 0, 21, 2,
        8,
        9,
    ];
    test_encode_decode(&instructions, &expected_bytes);
}

#[test]
fn skip() {
    #[derive(Debug, PartialEq, Eq, Bytecode)]
    struct Operand {
        #[bytecode(skip)]
        value1: String,
        value2: u8,
    }

    #[derive(Debug, PartialEq, Eq, Bytecode)]
    #[bytecode(type = u8)]
    enum Instruction {
        A(#[bytecode(skip)] bool),
        B {
            #[bytecode(skip)]
            value: bool,
        },
        C(u8, #[bytecode(skip)] String),
        D {
            #[bytecode(skip)]
            value1: usize,
            value2: u8,
            value3: u16,
        },
        E(Operand),
    }

    let instructions = [
        Instruction::A(true),
        Instruction::A(false),
        Instruction::B { value: true },
        Instruction::B { value: false },
        Instruction::C(10, "foo".to_owned()),
        Instruction::C(11, String::new()),
        Instruction::D {
            value1: 42,
            value2: 20,
            value3: 1,
        },
        Instruction::D {
            value1: 43,
            value2: 21,
            value3: 2,
        },
        Instruction::E(Operand {
            value1: "bar".to_owned(),
            value2: 1,
        }),
        Instruction::E(Operand {
            value1: "baz".to_owned(),
            value2: 2,
        }),
    ];
    #[rustfmt::skip]
    let expected_bytes = [
        0,
        0,
        1,
        1,
        2, 10,
        2, 11,
        3, 20, 0, 1,
        3, 21, 0, 2,
        4, 1,
        4, 2,
    ];
    test_encode(&instructions, &expected_bytes);

    let expected_instructions = [
        Instruction::A(false),
        Instruction::A(false),
        Instruction::B { value: false },
        Instruction::B { value: false },
        Instruction::C(10, String::new()),
        Instruction::C(11, String::new()),
        Instruction::D {
            value1: 0,
            value2: 20,
            value3: 1,
        },
        Instruction::D {
            value1: 0,
            value2: 21,
            value3: 2,
        },
        Instruction::E(Operand {
            value1: String::new(),
            value2: 1,
        }),
        Instruction::E(Operand {
            value1: String::new(),
            value2: 2,
        }),
    ];
    test_decode(&expected_bytes, &expected_instructions);
}
