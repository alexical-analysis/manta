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

Block_2:                                          ; preds = %entry
  call void @fmt_println(i64 0)
  br label %Block_4

Block_3:                                          ; preds = %Block_4, %entry
  store i64 9, ptr %nine, align 8
  %load1 = load i64, ptr %nine, align 8
  %sgt = icmp sgt i64 %load1, 10
  br i1 %sgt, label %Block_5, label %Block_8

Block_4:                                          ; preds = %Block_2
  br label %Block_3

Block_5:                                          ; preds = %Block_3
  call void @fmt_println(i64 0)
  br label %Block_7

Block_6:                                          ; preds = %Block_9, %Block_7
  ret void

Block_7:                                          ; preds = %Block_5
  br label %Block_6

Block_8:                                          ; preds = %Block_3
  call void @fmt_println(i64 0)
  br label %Block_9

Block_9:                                          ; preds = %Block_8
  br label %Block_6
}
