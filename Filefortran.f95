module Filefortran
    implicit none
    public :: file_exist

contains

function file_exist(filename)
! ファイルの存否を出力
! Args:
!     filename (character(len = *), intent(in)) : ファイル名
! Returns:
!     (logical) : 存在する場合 .true.
    implicit none
    character(len = *), intent(in) :: filename
    logical :: file_exist
    integer :: unit, ios

    open(newunit = unit, file = filename, status = "old", action = "read", iostat = ios)
        file_exist = (ios /= 0)
    close(unit)
end function file_exist

subroutine num_line(filename, max_length, line_num)
! ファイルの行数をカウント
! Args:
!     filename (character(len = *), intent(in)) : ファイル名
!     max_length (integer, intent(in)) : 行に含まれる最大許容文字数
!     line_num (integer) : 行数
    implicit none
    character(len = *), intent(in) :: filename
    integer, intent(in) :: max_length
    character(len = max_length) :: line
    integer :: line_num

    integer :: unit, ios

    if (file_exist(filename) .eqv. .false.) then
        print *, "Couldn't find ", filename
        stop
    endif

    line_num = 0
    open(newunit = unit, file = filename, status = "old", action = "read", iostat = ios)
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1
        enddo
    close(unit)

end subroutine num_line


end module Filefortran