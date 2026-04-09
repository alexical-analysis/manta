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
  br i1 false, label %Block_5, label %Block_7
  unreachable
  %load2 = load ptr, ptr %p, align 8
  store i32 42, ptr %load2, align 4
  %load3 = load ptr, ptr %p, align 8
  unreachable
  %load4 = load ptr, ptr %p1, align 8
  store ptr %load4, ptr %p, align 8
  br label %Block_6
  br label %Block_4
  %load5 = load ptr, ptr %panic, align 8
  br label %Block_8
  br label %Block_4

Block_2:                                          ; preds = %entry

Block_3:                                          ; preds = %entry

Block_4:                                          ; preds = %entry, %entry

Block_5:                                          ; preds = %entry

Block_6:                                          ; preds = %entry

Block_7:                                          ; preds = %entry

Block_8:                                          ; preds = %entry
}

define void @main() {
entry:
  %p = alloca ptr, align 8
  switch i1 false, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]
  ret void
  %load = load ptr, ptr %p, align 8
  br label %Block_4
  br label %Block_2
  br label %Block_6
  br label %Block_2
  unreachable

Block_2:                                          ; preds = %entry, %entry

Block_3:                                          ; preds = %entry

Block_4:                                          ; preds = %entry

Block_5:                                          ; preds = %entry

Block_6:                                          ; preds = %entry

Block_7:                                          ; preds = %entry
}
