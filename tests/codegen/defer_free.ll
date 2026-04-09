; ModuleID = 'defer_free'
source_filename = "defer_free"

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

define { i8, [8 x i8] } @os_write({} %0, { i64, i64, ptr } %1) {
entry:
  %f = alloca {}, align 8
  %buf = alloca { i64, i64, ptr }, align 8
  unreachable
}

define { i8, [16 x i8] } @write_and_cleanup({ i64, ptr } %0) {
entry:
  %path = alloca { i64, ptr }, align 8
  %f = alloca {}, align 8
  %f1 = alloca {}, align 8
  %"<wrap>" = alloca { i8, [0 x i8] }, align 8
  %buf = alloca ptr, align 8
  %buf2 = alloca ptr, align 8
  %panic = alloca ptr, align 8
  %n = alloca i64, align 8
  %n3 = alloca i64, align 8
  %"<wrap>4" = alloca { i8, [8 x i8] }, align 8
  %"<defer>" = alloca { i8, [16 x i8] }, align 8
  %load = load { i64, ptr }, ptr %path, align 8
  switch i1 false, label %Block_5 [
    i8 0, label %Block_3
  ]
  br i1 false, label %Block_10, label %Block_12
  %load5 = load {}, ptr %f1, align 1
  store {} %load5, ptr %f, align 1
  br label %Block_4
  br label %Block_2
  %load6 = load { i8, [0 x i8] }, ptr %"<wrap>", align 1
  br label %Block_23
  %load7 = load {}, ptr %f, align 1
  %load8 = load ptr, ptr %buf, align 8
  %load9 = load { i64, i64, ptr }, ptr %load8, align 8
  switch i1 false, label %Block_19 [
    i8 0, label %Block_17
  ]
  %load10 = load ptr, ptr %buf2, align 8
  store ptr %load10, ptr %buf, align 8
  br label %Block_11
  br label %Block_9
  %load11 = load ptr, ptr %panic, align 8
  br label %Block_13
  br label %Block_9
  br label %Block_23
  %load12 = load i64, ptr %n3, align 8
  store i64 %load12, ptr %n, align 8
  br label %Block_18
  br label %Block_16
  %load13 = load { i8, [8 x i8] }, ptr %"<wrap>4", align 1
  br label %Block_23
  %load14 = load ptr, ptr %buf, align 8
  br label %Block_24
  br label %Block_27
  %load15 = load {}, ptr %f, align 1
  br label %Block_28
  br label %Block_30
  %load16 = load { i8, [16 x i8] }, ptr %"<defer>", align 1
  ret { i8, [16 x i8] } %load16

Block_2:                                          ; preds = %entry

Block_3:                                          ; preds = %entry

Block_4:                                          ; preds = %entry

Block_5:                                          ; preds = %entry

Block_9:                                          ; preds = %entry, %entry

Block_10:                                         ; preds = %entry

Block_11:                                         ; preds = %entry

Block_12:                                         ; preds = %entry

Block_13:                                         ; preds = %entry

Block_16:                                         ; preds = %entry

Block_17:                                         ; preds = %entry

Block_18:                                         ; preds = %entry

Block_19:                                         ; preds = %entry

Block_23:                                         ; preds = %entry, %entry, %entry

Block_24:                                         ; preds = %entry

Block_27:                                         ; preds = %entry

Block_28:                                         ; preds = %entry

Block_30:                                         ; preds = %entry
}
