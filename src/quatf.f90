module quatf
   use iso_fortran_env, only: sp => real32, dp => real64, qp => real128
   implicit none
   private
   public quaternion
   public operator(+), operator(-), operator(*), operator(/)
   public operator(.ne.), operator(.eq.)
   public conjg, abs
   public write(formatted)

   character(*), parameter :: fmt = &
      '(g0, sp, g0, "i", g0, "j", g0, "k")'

   type quaternion(wp)
      integer, kind :: wp = KIND(0.0)
      real(wp) :: e,i,j,k
   end type

   interface operator(+)
      elemental module function quaternion_addition_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs, rhs
         type(quaternion(sp)) :: ret
      end function
      elemental module function quaternion_addition_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs, rhs
         type(quaternion(dp)) :: ret
      end function
      elemental module function quaternion_addition_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs, rhs
         type(quaternion(qp)) :: ret
      end function
   end interface operator(+)

   interface operator(-)
      elemental module function quaternion_subtraction_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs, rhs
         type(quaternion(sp)) :: ret
      end function
      elemental module function quaternion_subtraction_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs, rhs
         type(quaternion(dp)) :: ret
      end function
      elemental module function quaternion_subtraction_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs, rhs
         type(quaternion(qp)) :: ret
      end function
   end interface operator(-)

   interface operator(-)
      elemental module function quaternion_negative_sp(quat) result(ret)
         type(quaternion(sp)), intent(in) :: quat
         type(quaternion(sp)) :: ret
      end function
      elemental module function quaternion_negative_dp(quat) result(ret)
         type(quaternion(dp)), intent(in) :: quat
         type(quaternion(dp)) :: ret
      end function
      elemental module function quaternion_negative_qp(quat) result(ret)
         type(quaternion(qp)), intent(in) :: quat
         type(quaternion(qp)) :: ret
      end function
   end interface operator(-)

   interface operator(*)
      elemental module function quaternion_scalar_lmul_sp(lhs, rhs) result(ret)
         real(sp), intent(in) :: lhs
         type(quaternion(sp)), intent(in) :: rhs
         type(quaternion(sp)) :: ret
      end function
      elemental module function quaternion_scalar_rmul_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs
         real(sp), intent(in) :: rhs
         type(quaternion(sp)) :: ret
      end function
      elemental module function quaternion_multiplication_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs, rhs
         type(quaternion(sp)) :: ret
      end function

      elemental module function quaternion_scalar_lmul_dp(lhs, rhs) result(ret)
         real(dp), intent(in) :: lhs
         type(quaternion(dp)), intent(in) :: rhs
         type(quaternion(dp)) :: ret
      end function
      elemental module function quaternion_scalar_rmul_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         type(quaternion(dp)) :: ret
      end function
      elemental module function quaternion_multiplication_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs, rhs
         type(quaternion(dp)) :: ret
      end function

      elemental module function quaternion_scalar_lmul_qp(lhs, rhs) result(ret)
         real(qp), intent(in) :: lhs
         type(quaternion(qp)), intent(in) :: rhs
         type(quaternion(qp)) :: ret
      end function
      elemental module function quaternion_scalar_rmul_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs
         real(qp), intent(in) :: rhs
         type(quaternion(qp)) :: ret
      end function
      elemental module function quaternion_multiplication_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs, rhs
         type(quaternion(qp)) :: ret
      end function
   end interface operator(*)

   interface operator(/)
      elemental module function quaternion_scalar_div_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs
         real(sp), intent(in) :: rhs
         type(quaternion(sp)) :: ret
      end function
      elemental module function quaternion_division_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs, rhs
         type(quaternion(sp)) :: ret
      end function

      elemental module function quaternion_scalar_div_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs
         real(dp), intent(in) :: rhs
         type(quaternion(dp)) :: ret
      end function
      elemental module function quaternion_division_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs, rhs
         type(quaternion(dp)) :: ret
      end function

      elemental module function quaternion_scalar_div_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs
         real(qp), intent(in) :: rhs
         type(quaternion(qp)) :: ret
      end function
      elemental module function quaternion_division_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs, rhs
         type(quaternion(qp)) :: ret
      end function
   end interface operator(/)

   interface operator(.eq.)
      elemental module function quaternion_equal_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs, rhs
         logical :: ret
      end function
      elemental module function quaternion_equal_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs, rhs
         logical :: ret
      end function
      elemental module function quaternion_equal_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs, rhs
         logical :: ret
      end function
   end interface operator(.eq.)

   interface operator(.ne.)
      elemental module function quaternion_nequal_sp(lhs, rhs) result(ret)
         type(quaternion(sp)), intent(in) :: lhs, rhs
         logical :: ret
      end function
      elemental module function quaternion_nequal_dp(lhs, rhs) result(ret)
         type(quaternion(dp)), intent(in) :: lhs, rhs
         logical :: ret
      end function
      elemental module function quaternion_nequal_qp(lhs, rhs) result(ret)
         type(quaternion(qp)), intent(in) :: lhs, rhs
         logical :: ret
      end function
   end interface operator(.ne.)

   interface abs
      elemental module function quaternion_abs_sp(quat) result(ret)
         type(quaternion(sp)), intent(in) :: quat
         real(sp) :: ret
      end function
      elemental module function quaternion_abs_dp(quat) result(ret)
         type(quaternion(dp)), intent(in) :: quat
         real(dp) :: ret
      end function
      elemental module function quaternion_abs_qp(quat) result(ret)
         type(quaternion(qp)), intent(in) :: quat
         real(qp) :: ret
      end function
   end interface

   interface conjg
      elemental module function quaternion_conjg_sp(quat) result(ret)
         type(quaternion(sp)), intent(in) :: quat
         type(quaternion(sp)) :: ret
      end function
      elemental module function quaternion_conjg_dp(quat) result(ret)
         type(quaternion(dp)), intent(in) :: quat
         type(quaternion(dp)) :: ret
      end function
      elemental module function quaternion_conjg_qp(quat) result(ret)
         type(quaternion(qp)), intent(in) :: quat
         type(quaternion(qp)) :: ret
      end function
   end interface

   interface write(formatted)
      module subroutine write_formatted_sp(quat, unit, iotype, v_list, iostat, iomsg)
         class(quaternion(sp)), intent(in) :: quat
         integer, intent(in) :: unit
         character(len=*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(len=*), intent(inout) :: iomsg
      end subroutine
      module subroutine write_formatted_dp(quat, unit, iotype, v_list, iostat, iomsg)
         class(quaternion(dp)), intent(in) :: quat
         integer, intent(in) :: unit
         character(len=*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(len=*), intent(inout) :: iomsg
      end subroutine
      module subroutine write_formatted_qp(quat, unit, iotype, v_list, iostat, iomsg)
         class(quaternion(qp)), intent(in) :: quat
         integer, intent(in) :: unit
         character(len=*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(len=*), intent(inout) :: iomsg
      end subroutine
   end interface

contains

   module procedure quaternion_addition_sp
      ret%e = lhs%e + rhs%e
      ret%i = lhs%i + rhs%i
      ret%j = lhs%j + rhs%j
      ret%k = lhs%k + rhs%k
   end procedure
   module procedure quaternion_addition_dp
      ret%e = lhs%e + rhs%e
      ret%i = lhs%i + rhs%i
      ret%j = lhs%j + rhs%j
      ret%k = lhs%k + rhs%k
   end procedure
   module procedure quaternion_addition_qp
      ret%e = lhs%e + rhs%e
      ret%i = lhs%i + rhs%i
      ret%j = lhs%j + rhs%j
      ret%k = lhs%k + rhs%k
   end procedure

   module procedure quaternion_subtraction_sp
      ret%e = lhs%e - rhs%e
      ret%i = lhs%i - rhs%i
      ret%j = lhs%j - rhs%j
      ret%k = lhs%k - rhs%k
   end procedure
   module procedure quaternion_subtraction_dp
      ret%e = lhs%e - rhs%e
      ret%i = lhs%i - rhs%i
      ret%j = lhs%j - rhs%j
      ret%k = lhs%k - rhs%k
   end procedure
   module procedure quaternion_subtraction_qp
      ret%e = lhs%e - rhs%e
      ret%i = lhs%i - rhs%i
      ret%j = lhs%j - rhs%j
      ret%k = lhs%k - rhs%k
   end procedure

   module procedure quaternion_negative_sp
      ret%e = -quat%e
      ret%i = -quat%i
      ret%j = -quat%j
      ret%k = -quat%k
   end procedure
   module procedure quaternion_negative_dp
      ret%e = -quat%e
      ret%i = -quat%i
      ret%j = -quat%j
      ret%k = -quat%k
   end procedure
   module procedure quaternion_negative_qp
      ret%e = -quat%e
      ret%i = -quat%i
      ret%j = -quat%j
      ret%k = -quat%k
   end procedure


   module procedure quaternion_scalar_lmul_sp
      ret%e = lhs*rhs%e
      ret%i = lhs*rhs%i
      ret%j = lhs*rhs%j
      ret%k = lhs*rhs%k
   end procedure
   module procedure quaternion_scalar_rmul_sp
      ret%e = lhs%e*rhs
      ret%i = lhs%i*rhs
      ret%j = lhs%j*rhs
      ret%k = lhs%k*rhs
   end procedure
   module procedure quaternion_multiplication_sp
      ret%e = lhs%e*rhs%e - lhs%i*rhs%i - lhs%j*rhs%j - lhs%k*rhs%k
      ret%i = lhs%e*rhs%i + lhs%i*rhs%e + lhs%j*rhs%k - lhs%k*rhs%j
      ret%j = lhs%e*rhs%j - lhs%i*rhs%k + lhs%j*rhs%e + lhs%k*rhs%i
      ret%k = lhs%e*rhs%k + lhs%i*rhs%j - lhs%j*rhs%i + lhs%k*rhs%e
   end procedure

   module procedure quaternion_scalar_lmul_dp
      ret%e = lhs*rhs%e
      ret%i = lhs*rhs%i
      ret%j = lhs*rhs%j
      ret%k = lhs*rhs%k
   end procedure
   module procedure quaternion_scalar_rmul_dp
      ret%e = lhs%e*rhs
      ret%i = lhs%i*rhs
      ret%j = lhs%j*rhs
      ret%k = lhs%k*rhs
   end procedure
   module procedure quaternion_multiplication_dp
      ret%e = lhs%e*rhs%e - lhs%i*rhs%i - lhs%j*rhs%j - lhs%k*rhs%k
      ret%i = lhs%e*rhs%i + lhs%i*rhs%e + lhs%j*rhs%k - lhs%k*rhs%j
      ret%j = lhs%e*rhs%j - lhs%i*rhs%k + lhs%j*rhs%e + lhs%k*rhs%i
      ret%k = lhs%e*rhs%k + lhs%i*rhs%j - lhs%j*rhs%i + lhs%k*rhs%e
   end procedure

   module procedure quaternion_scalar_lmul_qp
      ret%e = lhs*rhs%e
      ret%i = lhs*rhs%i
      ret%j = lhs*rhs%j
      ret%k = lhs*rhs%k
   end procedure
   module procedure quaternion_scalar_rmul_qp
      ret%e = lhs%e*rhs
      ret%i = lhs%i*rhs
      ret%j = lhs%j*rhs
      ret%k = lhs%k*rhs
   end procedure
   module procedure quaternion_multiplication_qp
      ret%e = lhs%e*rhs%e - lhs%i*rhs%i - lhs%j*rhs%j - lhs%k*rhs%k
      ret%i = lhs%e*rhs%i + lhs%i*rhs%e + lhs%j*rhs%k - lhs%k*rhs%j
      ret%j = lhs%e*rhs%j - lhs%i*rhs%k + lhs%j*rhs%e + lhs%k*rhs%i
      ret%k = lhs%e*rhs%k + lhs%i*rhs%j - lhs%j*rhs%i + lhs%k*rhs%e
   end procedure


   module procedure quaternion_scalar_div_sp
      ret%e = lhs%e / rhs
      ret%e = lhs%i / rhs
      ret%e = lhs%j / rhs
      ret%e = lhs%k / rhs
   end procedure
   module procedure quaternion_division_sp
      ret = lhs * conjg(rhs) / (rhs%e**2 + rhs%i**2 + rhs%j**2 + rhs%k**2)
   end procedure
   module procedure quaternion_scalar_div_dp
      ret%e = lhs%e / rhs
      ret%e = lhs%i / rhs
      ret%e = lhs%j / rhs
      ret%e = lhs%k / rhs
   end procedure
   module procedure quaternion_division_dp
      ret = lhs * conjg(rhs) / (rhs%e**2 + rhs%i**2 + rhs%j**2 + rhs%k**2)
   end procedure
   module procedure quaternion_scalar_div_qp
      ret%e = lhs%e / rhs
      ret%e = lhs%i / rhs
      ret%e = lhs%j / rhs
      ret%e = lhs%k / rhs
   end procedure
   module procedure quaternion_division_qp
      ret = lhs * conjg(rhs) / (rhs%e**2 + rhs%i**2 + rhs%j**2 + rhs%k**2)
   end procedure

   module procedure quaternion_equal_sp
      ret = lhs%e.eq.rhs%e .and. lhs%i.eq.rhs%i .and. lhs%j.eq.rhs%j .and. lhs%k.eq.rhs%k
   end procedure
   module procedure quaternion_equal_dp
      ret = lhs%e.eq.rhs%e .and. lhs%i.eq.rhs%i .and. lhs%j.eq.rhs%j .and. lhs%k.eq.rhs%k
   end procedure
   module procedure quaternion_equal_qp
      ret = lhs%e.eq.rhs%e .and. lhs%i.eq.rhs%i .and. lhs%j.eq.rhs%j .and. lhs%k.eq.rhs%k
   end procedure

   module procedure quaternion_nequal_sp
      ret = lhs%e.ne.rhs%e .or. lhs%i.ne.rhs%i .or. lhs%j.ne.rhs%j .or. lhs%k.ne.rhs%k
   end procedure
   module procedure quaternion_nequal_dp
      ret = lhs%e.ne.rhs%e .or. lhs%i.ne.rhs%i .or. lhs%j.ne.rhs%j .or. lhs%k.ne.rhs%k
   end procedure
   module procedure quaternion_nequal_qp
      ret = lhs%e.ne.rhs%e .or. lhs%i.ne.rhs%i .or. lhs%j.ne.rhs%j .or. lhs%k.ne.rhs%k
   end procedure

   module procedure quaternion_abs_sp
      ret = sqrt(quat%e**2 + quat%i**2 + quat%j**2 + quat%k**2)
   end procedure
   module procedure quaternion_abs_dp
      ret = sqrt(quat%e**2 + quat%i**2 + quat%j**2 + quat%k**2)
   end procedure
   module procedure quaternion_abs_qp
      ret = sqrt(quat%e**2 + quat%i**2 + quat%j**2 + quat%k**2)
   end procedure

   module procedure quaternion_conjg_sp
      ret = quaternion(sp, quat%e, -quat%i, -quat%j, -quat%k)
   end procedure
   module procedure quaternion_conjg_dp
      ret = quaternion(dp, quat%e, -quat%i, -quat%j, -quat%k)
   end procedure
   module procedure quaternion_conjg_qp
      ret = quaternion(qp, quat%e, -quat%i, -quat%j, -quat%k)
   end procedure

   module procedure write_formatted_sp
      write(unit, fmt, iostat=iostat, iomsg=iomsg) quat%e, quat%i, quat%j, quat%k
   end procedure
   module procedure write_formatted_dp
      write(unit, fmt, iostat=iostat, iomsg=iomsg) quat%e, quat%i, quat%j, quat%k
   end procedure
   module procedure write_formatted_qp
      write(unit, fmt, iostat=iostat, iomsg=iomsg) quat%e, quat%i, quat%j, quat%k
   end procedure

end module quatf
