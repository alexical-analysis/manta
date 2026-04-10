; ModuleID = 'missing_module'
source_filename = "missing_module"

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  ret void
}

define void @main() {
entry:
  call void @fmt_println(i64 0)
  ret void
}
