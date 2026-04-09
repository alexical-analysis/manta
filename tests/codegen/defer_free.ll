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
  switch i64 0, label %Block_5 [
    i8 0, label %Block_3
  ]

Block_2:                                          ; preds = %Block_4
  br i1 false, label %Block_10, label %Block_12

Block_3:                                          ; preds = %entry
  %load5 = load {}, ptr %f1, align 1
  %load6 = load {}, ptr %f1, align 1
  store {} %load6, ptr %f, align 1
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  %load7 = load { i8, [0 x i8] }, ptr %"<wrap>", align 1
  br label %Block_23

Block_9:                                          ; preds = %Block_13, %Block_11
  %load8 = load {}, ptr %f, align 1
  %deref = load ptr, ptr %buf, align 8
  %load9 = load { i64, i64, ptr }, ptr %deref, align 8
  switch i64 0, label %Block_19 [
    i8 0, label %Block_17
  ]

Block_10:                                         ; preds = %Block_2
  %load10 = load ptr, ptr %buf2, align 8
  %load11 = load ptr, ptr %buf2, align 8
  store ptr %load11, ptr %buf, align 8
  br label %Block_11

Block_11:                                         ; preds = %Block_10
  br label %Block_9

Block_12:                                         ; preds = %Block_2
  %load12 = load ptr, ptr %panic, align 8
  br label %Block_13

Block_13:                                         ; preds = %Block_12
  br label %Block_9

Block_16:                                         ; preds = %Block_18
  br label %Block_23

Block_17:                                         ; preds = %Block_9
  %load13 = load i64, ptr %n3, align 8
  %load14 = load i64, ptr %n3, align 8
  store i64 %load14, ptr %n, align 8
  br label %Block_18

Block_18:                                         ; preds = %Block_17
  br label %Block_16

Block_19:                                         ; preds = %Block_9
  %load15 = load { i8, [8 x i8] }, ptr %"<wrap>4", align 1
  br label %Block_23

Block_23:                                         ; preds = %Block_19, %Block_16, %Block_5
  %load16 = load ptr, ptr %buf, align 8
  br label %Block_24

Block_24:                                         ; preds = %Block_23
  br label %Block_27

Block_27:                                         ; preds = %Block_24
  %load17 = load {}, ptr %f, align 1
  br label %Block_28

Block_28:                                         ; preds = %Block_27
  br label %Block_30

Block_30:                                         ; preds = %Block_28
  %load18 = load { i8, [16 x i8] }, ptr %"<defer>", align 1
  ret { i8, [16 x i8] } %load18
}
