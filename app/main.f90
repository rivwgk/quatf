! SPDX-Identifier: Apache-2.0

program main
   use iso_fortran_env, only: sp => real32
   use quatf
   implicit none
   type(quaternion(sp)) :: q1, q2, q3, q4

   !> constructing a single precision quaternion describing 1 + 2i - 2j + k
   q1 = quaternion(sp, 1.0, 2.0, -2.0, 1.0)
   !> q2 is the scalar part of q1 and q3 the vector part of q1
   q2 = 0.5 * (q1 + conjg(q1))
   q3 = 0.5 * (q1 - conjg(q1))
   !> q4 is the normalized q1
   q4 = q1 / abs(q1)

end program main
