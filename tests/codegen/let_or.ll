; ModuleID = 'let_or'
source_filename = "let_or"

define void @"<init>"() {
entry:
  ret void
}

define { i8, [0 x i8] } @os_open({ i64, ptr } %0) {
entry:
  %path = alloca { i64, ptr }, align 8
  unreachable
}

define void @os_close({} %0) {
entry:
  %f = alloca {}, align 8
  ret void
}

define { i8, [16 x i8] } @io_read_to_string({} %0) {
entry:
  %f = alloca {}, align 8
  unreachable
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  ret void
}

define { i8, [24 x i8] } @read_file({ i64, ptr } %0) {
entry:
  %path = alloca { i64, ptr }, align 8
  %f = alloca {}, align 8
  %f1 = alloca {}, align 8
  %"<wrap>" = alloca { i8, [0 x i8] }, align 8
  %s = alloca { i64, ptr }, align 8
  %s2 = alloca { i64, ptr }, align 8
  %"<wrap>3" = alloca { i8, [16 x i8] }, align 8
  %"<defer>" = alloca { i8, [24 x i8] }, align 8
  %load = load { i64, ptr }, ptr %path, align 8
  switch i64 0, label %Block_5 [
    i8 0, label %Block_3
  ]

Block_2:                                          ; preds = %Block_4
  %load4 = load {}, ptr %f, align 1
  switch i64 0, label %Block_12 [
    i8 0, label %Block_10
  ]

Block_3:                                          ; preds = %entry
  %load5 = load {}, ptr %f1, align 1
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  %load6 = load { i8, [0 x i8] }, ptr %"<wrap>", align 1
  br label %Block_16

Block_9:                                          ; preds = %Block_11
  %load7 = load { i64, ptr }, ptr %s, align 8
  br label %Block_16

Block_10:                                         ; preds = %Block_2
  %load8 = load { i64, ptr }, ptr %s2, align 8
  br label %Block_11

Block_11:                                         ; preds = %Block_10
  br label %Block_9

Block_12:                                         ; preds = %Block_2
  %load9 = load { i8, [16 x i8] }, ptr %"<wrap>3", align 1
  br label %Block_16

Block_16:                                         ; preds = %Block_12, %Block_9, %Block_5
  %load10 = load {}, ptr %f, align 1
  br label %Block_17

Block_17:                                         ; preds = %Block_16
  br label %Block_19

Block_19:                                         ; preds = %Block_17
  %load11 = load { i8, [24 x i8] }, ptr %"<defer>", align 1
  ret { i8, [24 x i8] } %load11
}

define void @main() {
entry:
  %content = alloca { i64, ptr }, align 8
  %content1 = alloca { i64, ptr }, align 8
  %e = alloca { i8, [24 x i8] }, align 8
  %content2 = alloca { i64, ptr }, align 8
  %content3 = alloca { i64, ptr }, align 8
  %panic = alloca { i8, [24 x i8] }, align 8
  switch i64 0, label %Block_5 [
    i8 0, label %Block_3
  ]

Block_2:                                          ; preds = %Block_4
  %load = load { i64, ptr }, ptr %content, align 8
  switch i64 0, label %Block_10 [
    i8 0, label %Block_8
  ]

Block_3:                                          ; preds = %entry
  %load4 = load { i64, ptr }, ptr %content1, align 8
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  ret void

Block_7:                                          ; preds = %Block_11, %Block_9
  %load5 = load { i64, ptr }, ptr %content2, align 8
  ret void

Block_8:                                          ; preds = %Block_2
  %load6 = load { i64, ptr }, ptr %content3, align 8
  br label %Block_9

Block_9:                                          ; preds = %Block_8
  br label %Block_7

Block_10:                                         ; preds = %Block_2
  %load7 = load { i8, [24 x i8] }, ptr %panic, align 1
  br label %Block_11

Block_11:                                         ; preds = %Block_10
  br label %Block_7
}
