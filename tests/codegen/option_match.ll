; ModuleID = 'option_match'
source_filename = "option_match"

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  ret void
}

define { i8, [4 x i8] } @div(i32 %0, i32 %1) {
entry:
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  %load = load i32, ptr %b, align 4
  %load1 = load i32, ptr %b, align 4
  %fneq = icmp eq i32 %load1, 0
  br i1 %fneq, label %Block_2, label %Block_3

Block_2:                                          ; preds = %entry
  unreachable

Block_3:                                          ; preds = %entry
  %load2 = load i32, ptr %a, align 4
  %load3 = load i32, ptr %b, align 4
  %load4 = load i32, ptr %a, align 4
  %load5 = load i32, ptr %b, align 4
  %sdiv = sdiv i32 %load4, %load5
  unreachable
}

define void @main() {
entry:
  %r = alloca { i8, [4 x i8] }, align 8
  %v = alloca i32, align 4
  %load = load { i8, [4 x i8] }, ptr %r, align 1
  switch i64 0, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]

Block_2:                                          ; preds = %Block_6, %Block_4
  ret void

Block_3:                                          ; preds = %entry
  %load1 = load i32, ptr %v, align 4
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
