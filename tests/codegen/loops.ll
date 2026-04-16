; ModuleID = 'loops'
source_filename = "loops"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1

declare ptr @malloc(i64)

declare void @free(ptr)

declare i32 @puts(ptr)

declare void @abort()

declare i64 @write(i32, ptr, i64)

define internal void @panic() {
entry:
  %puts = call i32 @puts(ptr @panic_msg)
  call void @abort()
  unreachable
}

define internal ptr @alloc({ i64, i64, i64 } %0) {
entry:
  %meta_size = extractvalue { i64, i64, i64 } %0, 0
  %malloc = call ptr @malloc(i64 %meta_size)
  ret ptr %malloc
}

define internal void @print({ i64, ptr } %0) {
entry:
  %len = extractvalue { i64, ptr } %0, 0
  %ptr = extractvalue { i64, ptr } %0, 1
  %write = call i64 @write(i32 1, ptr %ptr, i64 %len)
  ret void
}

define internal void @eprint({ i64, ptr } %0) {
entry:
  %len = extractvalue { i64, ptr } %0, 0
  %ptr = extractvalue { i64, ptr } %0, 1
  %write = call i64 @write(i32 2, ptr %ptr, i64 %len)
  ret void
}

define void @"<init>"() {
entry:
  ret void
}

define i64 @main() {
entry:
  %a = alloca i64, align 8
  %b = alloca i1, align 1
  %c = alloca i1, align 1
  %d = alloca double, align 8
  store i64 0, ptr %a, align 8
  store i1 true, ptr %b, align 1
  br label %Block_2

Block_2:                                          ; preds = %Block_15, %Block_3, %entry
  %load = load i1, ptr %b, align 1
  br i1 %load, label %Block_3, label %Block_4

Block_3:                                          ; preds = %Block_2
  store i1 false, ptr %b, align 1
  br label %Block_2

Block_4:                                          ; preds = %Block_2
  %load1 = load i64, ptr %a, align 8
  %iadd = add i64 %load1, 1
  store i64 %iadd, ptr %a, align 8
  %load2 = load i64, ptr %a, align 8
  %sgt = icmp sgt i64 %load2, 255
  br i1 %sgt, label %Block_6, label %Block_7

Block_6:                                          ; preds = %Block_4
  br label %Block_16

Block_7:                                          ; preds = %Block_4
  br label %Block_9

Block_9:                                          ; preds = %Block_7
  br label %Block_10

Block_10:                                         ; preds = %Block_9
  br label %Block_12

Block_12:                                         ; preds = %Block_10
  br label %Block_14

Block_14:                                         ; preds = %Block_12
  br label %Block_15

Block_15:                                         ; preds = %Block_14
  br label %Block_2

Block_16:                                         ; preds = %Block_6
  store i1 true, ptr %c, align 1
  store double 3.141500e+00, ptr %d, align 8
  br label %Block_17

Block_17:                                         ; preds = %Block_24, %Block_16
  %load3 = load i1, ptr %c, align 1
  %not = xor i1 %load3, true
  br i1 %not, label %Block_18, label %Block_19

Block_18:                                         ; preds = %Block_17
  br label %Block_25

Block_19:                                         ; preds = %Block_17
  %load4 = load double, ptr %d, align 8
  %fadd = fadd double %load4, 3.141500e+00
  store double %fadd, ptr %d, align 8
  %load5 = load double, ptr %d, align 8
  %fgt = fcmp ogt double %load5, 3.000000e+01
  br i1 %fgt, label %Block_21, label %Block_22

Block_21:                                         ; preds = %Block_19
  store i1 false, ptr %c, align 1
  br label %Block_23

Block_22:                                         ; preds = %Block_23, %Block_19
  br label %Block_24

Block_23:                                         ; preds = %Block_21
  br label %Block_22

Block_24:                                         ; preds = %Block_22
  br label %Block_17

Block_25:                                         ; preds = %Block_18
  %load6 = load i64, ptr %a, align 8
  ret i64 %load6
}
