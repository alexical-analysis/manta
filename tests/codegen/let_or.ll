; ModuleID = 'let_or'
source_filename = "let_or"

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

define { i8, [16 x i8] } @io_read_to_string({} %0) {
entry:
  %f = alloca {}, align 8
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
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
}

define void @main() {
entry:
  %content = alloca { i64, ptr }, align 8
  %content1 = alloca { i64, ptr }, align 8
  %e = alloca { i8, [24 x i8] }, align 8
  %content2 = alloca { i64, ptr }, align 8
  %content3 = alloca { i64, ptr }, align 8
  %panic = alloca { i8, [24 x i8] }, align 8

Block_2:                                          ; No predecessors!

Block_3:                                          ; No predecessors!

Block_4:                                          ; No predecessors!

Block_5:                                          ; No predecessors!

Block_6:                                          ; No predecessors!

Block_7:                                          ; No predecessors!

Block_8:                                          ; No predecessors!

Block_9:                                          ; No predecessors!

Block_10:                                         ; No predecessors!
}
