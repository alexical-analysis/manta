; ModuleID = 'hello'
source_filename = "hello"

@float_array = constant [3 x float] [float 1.000000e+00, float 8.000000e+00, float 4.200000e+01]

define double @average(ptr %0, i64 %1) {
entry:
  %sum = alloca double, align 8
  store double 0.000000e+00, ptr %sum, align 8
  %i = alloca i64, align 8
  store i64 0, ptr %i, align 8
  %count_is_zero = icmp eq i64 %1, 0
  br i1 %count_is_zero, label %ret, label %loop

loop:                                             ; preds = %loop, %entry
  %sum1 = load double, ptr %sum, align 8
  %i2 = load i64, ptr %i, align 8
  %elem_ptr = getelementptr float, ptr %0, i64 %i2
  %elem = load float, ptr %elem_ptr, align 4
  %elem_64_val = fpext float %elem to double
  %new_sum = fadd double %sum1, %elem_64_val
  store double %new_sum, ptr %sum, align 8
  %i_plus_one = add i64 %i2, 1
  store i64 %i_plus_one, ptr %i, align 8
  %loop_is_done = icmp uge i64 %i_plus_one, %1
  br i1 %loop_is_done, label %ret, label %loop

ret:                                              ; preds = %loop, %entry
  %float_count = uitofp i64 %1 to double
  %sum_value = load double, ptr %sum, align 8
  %ret3 = fdiv double %sum_value, %float_count
  ret double %ret3
}

define i64 @main() {
entry:
  %avg = call double @average(ptr @float_array, i64 3)
  %ret_cast = fptoui double %avg to i64
  ret i64 %ret_cast
}
