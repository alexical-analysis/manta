; ModuleID = 'none'
source_filename = "none"

define void @"<init>"() {
entry:
}

define void @fmt_println_ptr(ptr %0) {
entry:
  %p = alloca ptr, align 8
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
}

define { i8, [8 x i8] } @maybe_alloc(i1 %0) {
entry:
  %cond = alloca i1, align 1
  %p = alloca ptr, align 8
  %p1 = alloca ptr, align 8
  %panic = alloca ptr, align 8

Block_2:                                          ; No predecessors!

Block_3:                                          ; No predecessors!

Block_4:                                          ; No predecessors!

Block_5:                                          ; No predecessors!

Block_6:                                          ; No predecessors!

Block_7:                                          ; No predecessors!

Block_8:                                          ; No predecessors!
}

define void @main() {
entry:
  %p = alloca ptr, align 8

Block_2:                                          ; No predecessors!

Block_3:                                          ; No predecessors!

Block_4:                                          ; No predecessors!

Block_5:                                          ; No predecessors!

Block_6:                                          ; No predecessors!

Block_7:                                          ; No predecessors!
}
