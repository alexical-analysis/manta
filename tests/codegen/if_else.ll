; ModuleID = 'if_else'
source_filename = "if_else"

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
  %check_this = alloca i1, align 1
  %nine = alloca i64, align 8
  store i1 true, ptr %check_this, align 1
  %load = load i1, ptr %check_this, align 1
  br i1 %load, label %Block_2, label %Block_3
  br label %Block_4
  store i64 9, ptr %nine, align 8
  %load1 = load i64, ptr %nine, align 8
  %sgt = icmp sgt i64 %load1, 10
  br i1 %sgt, label %Block_5, label %Block_8
  br label %Block_3
  br label %Block_7
  ret void
  br label %Block_6
  br label %Block_9
  br label %Block_6

Block_2:                                          ; preds = %entry

Block_3:                                          ; preds = %entry, %entry

Block_4:                                          ; preds = %entry

Block_5:                                          ; preds = %entry

Block_6:                                          ; preds = %entry, %entry

Block_7:                                          ; preds = %entry

Block_8:                                          ; preds = %entry

Block_9:                                          ; preds = %entry
}
