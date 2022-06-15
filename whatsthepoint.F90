program whatsthepoint
  real (kind=8) :: Real8Var (10)
  integer :: k (10)
  integer :: i
 do i = 1,10
   Real8Var(i) = 100.0d0 + dble(i)
   k = int(i,8)
 enddo
 call pointing(Real8Var)
 call pointing(k)
      stop
contains
subroutine pointing (cvar)
  class(*), target :: cvar (:)
  real(kind=4), dimension (:), pointer :: r4point => null()
  real(kind=8), dimension (:), pointer :: r8point => null()
  integer (kind=4), dimension (:), pointer :: i4point => null()
  integer (kind=8), dimension (:), pointer :: i8point => null()

      select type (cvar)
        type is (real(kind=4))
           r4point => cvar
        type is (real(kind=8))
           r8point => cvar
        type is (integer(kind=4))
           i4point => cvar
        type is (integer(kind=8))
           i8point => cvar
      end select
  call using (r8point,r4point,i8point,i4point)
end subroutine pointing

subroutine using (r8,r4,i8,i4)
 
  real(kind=4), pointer :: r4 (:)
  real(kind=8), pointer :: r8 (:)
  integer (kind=4), pointer :: i4 (:)
  integer (kind=8), pointer :: i8 (:)

 if (associated(r4)) write (6,*) r4
 if (associated(r8)) write (6,*) r8
 if (associated(i4)) write (6,*) i4
 if (associated(i8)) write (6,*) i8

end subroutine using

end program whatsthepoint

