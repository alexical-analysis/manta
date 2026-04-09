; ModuleID = 'none'
source_filename = "none"

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println_ptr(ptr %0) {
entry:
  %p = alloca ptr, align 8
  ret void
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  ret void
}

define { i8, [8 x i8] } @maybe_alloc(i1 %0) {
entry:
  %cond = alloca i1, align 1
  %p = alloca ptr, align 8
  %p1 = alloca ptr, align 8
  %panic = alloca ptr, align 8
  %load = load i1, ptr %cond, align 1
  br i1 %load, label %Block_2, label %Block_3

Block_2:                                          ; preds = %entry
  br i1 false, label %Block_5, label %Block_7

Block_3:                                          ; preds = %entry
  unreachable

Block_4:                                          ; preds = %Block_8, %Block_6
  %load2 = load ptr, ptr %p, align 8
  unreachable

Block_5:                                          ; preds = %Block_2
  %load3 = load ptr, ptr %p1, align 8
  br label %Block_6

Block_6:                                          ; preds = %Block_5
  br label %Block_4

Block_7:                                          ; preds = %Block_2
  %load4 = load ptr, ptr %panic, align 8
  br label %Block_8

Block_8:                                          ; preds = %Block_7
  br label %Block_4
}

define void @main() {
entry:
  %p = alloca ptr, align 8
  switch i64 0, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]

Block_2:                                          ; preds = %Block_6, %Block_4
  ret void

Block_3:                                          ; preds = %entry
  %load = load ptr, ptr %p, align 8
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  br label %Block_6

Block_6:                                          ; preds = %Block_5
  br label %Block_2

Block_7:                                          ; preds = %entry
  unreachable
}
