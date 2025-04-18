module Charfortran
    implicit none
    public :: num_char, split_char, line2array
    
contains

function num_char(line, c)
! 文字列中に含まれる文字の数を出力
! Args:
!     line (character, intent(in)) : 文字列
!     c (character(len = 1), intent(in)) : 文字
! Returns
!     num_char (integer) : cの数
    implicit none
    character(len = 1), intent(in) :: c
    character(len = *), intent(in) :: line
    integer :: num_char
    integer pos

    num_char = 0
    pos = 1
    do pos = 1, len(trim(line))
        if (line(pos:pos) == c) num_char = num_char + 1
    enddo
end function num_char


function pos_char(line, c)
! 文字列中に含まれる文字の場所を出力
! Args:
!     line (character, intent(in)) : 文字列
!     c (character(len = 1), intent(in)) : 文字
! Returns
!     pos_char (integer(:), allocatable) : cのインデックス番号
    implicit none
    character(len = *), intent(in) :: line
    character(len = 1), intent(in) :: c
    integer, allocatable :: pos_char(:)
    integer :: num
    integer :: i, count

    num = num_char(line, c)
    allocate(pos_char(num))
    count = 1
    i = 1
    do
        if (line(i:i) == c) then
            pos_char(count) = i
            count = count + 1
        endif
        i = i+1
        if (count > num) exit
    enddo
end function pos_char


subroutine split_char(line, c, split_line)
! 文字列をcを区切り文字として分割
! Args:
!     line (character, intent(in)) : 文字列
!     c (character(len = 1), intent(in)) : 文字
!     split_line (character(len = *)(:), allocatable) : 分割後の文字列
    implicit none
    character(len = *), intent(in) :: line
    character(len = 1), intent(in) :: c
    character(len = *), allocatable :: split_line(:)
    integer, allocatable :: char_pos(:)
    integer :: st, pos_size
    integer :: i

    char_pos = pos_char(line, c)
    pos_size = size(char_pos)
    allocate(split_line(pos_size+1))

    st = 1
    do i = 1,pos_size
        split_line(i) = line(st:char_pos(i)-1)
        st = char_pos(i)+1
    enddo
    split_line(pos_size+1) = line(char_pos(pos_size)+1:)
end subroutine split_char

subroutine line2array(line, c, array)
! cで区切られた数値文字列を分割し、1次元配列に格納
! Args:
!     line (character(len = *), intent(in)) : 文字列
!     c (character(len = 1), intent(in)) : 区切り文字
!     array (real(:), allocatable) : 格納先1次元配列
    implicit none
    character(len = *), intent(in) :: line
    character(len = len(line)) :: line_cpy
    character(len = 1), intent(in) :: c
    real, allocatable :: array(:)
    integer :: num
    integer :: i

    num = 0
    do i = 1,len(line)
        if (line(i:i) == c) then
            line_cpy(i:i) = ","
            num = num + 1
        else
            line_cpy(i:i) = line(i:i)
        endif
    enddo

    allocate(array(num+1))
    read(line_cpy,*) array
end subroutine Line2Array
end module Charfortran