use inkwell::{
    OptimizationLevel,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};

pub fn create_target_machine(target_triple: TargetTriple) -> TargetMachine {
    Target::initialize_all(&InitializationConfig::default());
    let target =
        Target::from_triple(&target_triple).expect("failed to get target from target triple");

    target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("failed to create target machine")
}
