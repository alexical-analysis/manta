; ModuleID = 'hello'
source_filename = "hello"

@float_array = constant [3 x float] [float 1.000000e+00, float 8.000000e+00, float 4.200000e+01]

define double @average(ptr %0, i64 %1) {
entry:
  %count_is_zero = icmp eq i64 %1, 0
  br i1 %count_is_zero, label %ret, label %loop

loop:                                             ; preds = %loop, %entry
  %i2 = phi i64 [ %i_plus_one, %loop ], [ 0, %entry ]
  %sum1 = phi double [ %new_sum, %loop ], [ 0.000000e+00, %entry ]
  %elem_ptr = getelementptr float, ptr %0, i64 %i2
  %elem = load float, ptr %elem_ptr, align 4
  %elem_64_val = fpext float %elem to double
  %new_sum = fadd double %sum1, %elem_64_val
  %i_plus_one = add i64 %i2, 1
  %loop_is_done.not = icmp ult i64 %i_plus_one, %1
  br i1 %loop_is_done.not, label %loop, label %ret

ret:                                              ; preds = %loop, %entry
  %sum_value = phi double [ %new_sum, %loop ], [ 0.000000e+00, %entry ]
  %float_count = uitofp i64 %1 to double
  %ret3 = fdiv double %sum_value, %float_count
  ret double %ret3
}

define i64 @main() {
entry:
  %avg = call double @average(ptr nonnull @float_array, i64 3)
  %ret_cast = fptoui double %avg to i64
  ret i64 %ret_cast
}
