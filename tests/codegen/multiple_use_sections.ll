; ModuleID = 'multiple_use_sections'
source_filename = "multiple_use_sections"

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
  ret void
}
