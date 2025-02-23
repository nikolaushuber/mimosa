(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   redefine_step_errors
   (run mimosa check %{dep:redefine_step.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff redefine_step_errors.expected redefine_step_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   multi_inputs_errors
   (run mimosa check %{dep:multi_inputs.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff multi_inputs_errors.expected multi_inputs_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   multi_def_errors
   (run mimosa check %{dep:multi_def.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff multi_def_errors.expected multi_def_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   cyclic_step_errors
   (run mimosa check %{dep:cyclic_step.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff cyclic_step_errors.expected cyclic_step_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   unused_input_errors
   (run mimosa check %{dep:unused_input.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff unused_input_errors.expected unused_input_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   missing_output_errors
   (run mimosa check %{dep:missing_output.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff missing_output_errors.expected missing_output_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   cyclic_in_steps_errors
   (run mimosa check %{dep:cyclic_in_steps.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff cyclic_in_steps_errors.expected cyclic_in_steps_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   type_error_errors
   (run mimosa check %{dep:type_error.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff type_error_errors.expected type_error_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   unnamed_output_errors
   (run mimosa check %{dep:unnamed_output.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff unnamed_output_errors.expected unnamed_output_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   incorrect_type_1_errors
   (run mimosa check %{dep:incorrect_type_1.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff incorrect_type_1_errors.expected incorrect_type_1_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   missing_type_in_proto_errors
   (run mimosa check %{dep:missing_type_in_proto.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff missing_type_in_proto_errors.expected missing_type_in_proto_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   nested_pre_errors
   (run mimosa check %{dep:nested_pre.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff nested_pre_errors.expected nested_pre_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   init_func_arg_errors
   (run mimosa check %{dep:init_func_arg.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff init_func_arg_errors.expected init_func_arg_errors))))

(rule
 (deps
  (package mimosa))
 (action
  (with-stderr-to
   init_output_errors
   (run mimosa check %{dep:init_output.mim}))))

(rule
 (alias runtest)
 (package mimosa)
 (action
  (progn
   (diff init_output_errors.expected init_output_errors))))

