; ModuleID = 'pointers'
source_filename = "pointers"

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println(i64 %0) {
entry:
  %i = alloca i64, align 8
  ret void
}

define void @main() {
entry:
  %i = alloca i64, align 8
  %j = alloca i64, align 8
  %p = alloca ptr, align 8
  ret void
}
