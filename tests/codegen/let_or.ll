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
  switch i1 false, label %Block_5 [
    i8 0, label %Block_3
  ]
  %load4 = load {}, ptr %f, align 1
  switch i1 false, label %Block_12 [
    i8 0, label %Block_10
  ]
  %load5 = load {}, ptr %f1, align 1
  store {} %load5, ptr %f, align 1
  br label %Block_4
  br label %Block_2
  %load6 = load { i8, [0 x i8] }, ptr %"<wrap>", align 1
  br label %Block_16
  %load7 = load { i64, ptr }, ptr %s, align 8
  br label %Block_16
  %load8 = load { i64, ptr }, ptr %s2, align 8
  store { i64, ptr } %load8, ptr %s, align 8
  br label %Block_11
  br label %Block_9
  %load9 = load { i8, [16 x i8] }, ptr %"<wrap>3", align 1
  br label %Block_16
  %load10 = load {}, ptr %f, align 1
  br label %Block_17
  br label %Block_19
  %load11 = load { i8, [24 x i8] }, ptr %"<defer>", align 1
  ret { i8, [24 x i8] } %load11

Block_2:                                          ; preds = %entry

Block_3:                                          ; preds = %entry

Block_4:                                          ; preds = %entry

Block_5:                                          ; preds = %entry

Block_9:                                          ; preds = %entry

Block_10:                                         ; preds = %entry

Block_11:                                         ; preds = %entry

Block_12:                                         ; preds = %entry

Block_16:                                         ; preds = %entry, %entry, %entry

Block_17:                                         ; preds = %entry

Block_19:                                         ; preds = %entry
}

define void @main() {
entry:
  %content = alloca { i64, ptr }, align 8
  %content1 = alloca { i64, ptr }, align 8
  %e = alloca { i8, [24 x i8] }, align 8
  %content2 = alloca { i64, ptr }, align 8
  %content3 = alloca { i64, ptr }, align 8
  %panic = alloca { i8, [24 x i8] }, align 8
  switch i1 false, label %Block_5 [
    i8 0, label %Block_3
  ]
  %load = load { i64, ptr }, ptr %content, align 8
  switch i1 false, label %Block_10 [
    i8 0, label %Block_8
  ]
  %load4 = load { i64, ptr }, ptr %content1, align 8
  store { i64, ptr } %load4, ptr %content, align 8
  br label %Block_4
  br label %Block_2
  ret void
  %load5 = load { i64, ptr }, ptr %content2, align 8
  ret void
  %load6 = load { i64, ptr }, ptr %content3, align 8
  store { i64, ptr } %load6, ptr %content2, align 8
  br label %Block_9
  br label %Block_7
  %load7 = load { i8, [24 x i8] }, ptr %panic, align 1
  br label %Block_11
  br label %Block_7

Block_2:                                          ; preds = %entry

Block_3:                                          ; preds = %entry

Block_4:                                          ; preds = %entry

Block_5:                                          ; preds = %entry

Block_7:                                          ; preds = %entry, %entry

Block_8:                                          ; preds = %entry

Block_9:                                          ; preds = %entry

Block_10:                                         ; preds = %entry

Block_11:                                         ; preds = %entry
}
