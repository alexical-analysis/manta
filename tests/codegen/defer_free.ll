; ModuleID = 'defer_free'
source_filename = "defer_free"

define void @"<init>"() {
entry:
}

define { i8, [0 x i8] } @os_open({ i64, ptr } %0) {
entry:
  %path = alloca { i64, ptr }, align 8
}

define void @os_close({} %0) {
entry:
  %f = alloca {}, align 8
}

define { i8, [8 x i8] } @os_write({} %0, { i64, i64, ptr } %1) {
entry:
  %f = alloca {}, align 8
  %buf = alloca { i64, i64, ptr }, align 8
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

Block_2:                                          ; No predecessors!

Block_3:                                          ; No predecessors!

Block_4:                                          ; No predecessors!

Block_5:                                          ; No predecessors!

Block_6:                                          ; No predecessors!

Block_7:                                          ; No predecessors!

Block_8:                                          ; No predecessors!

Block_9:                                          ; No predecessors!

Block_10:                                         ; No predecessors!

Block_11:                                         ; No predecessors!

Block_12:                                         ; No predecessors!

Block_13:                                         ; No predecessors!

Block_14:                                         ; No predecessors!

Block_15:                                         ; No predecessors!

Block_16:                                         ; No predecessors!

Block_17:                                         ; No predecessors!

Block_18:                                         ; No predecessors!

Block_19:                                         ; No predecessors!
}
