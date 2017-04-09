! 0 empty
! 1 wall
! 2 objective
! 3 crate
! 4 player
! 5 (3+2)  crate on objective
! 6 (4+2) player on crate

module modsokoban
 integer mappy(0:25,0:25),playerpos(0:1),level,levelplaying
 integer levi,levj

 logical gameover
end module

subroutine init
 use modsokoban
 call level1
 gameover =.false.
end

subroutine playerposition
use modsokoban
do1: do i=0,levi
   do j=0,levj
      if(mappy(i,j) == 4) then
         playerpos(0)=i
         playerpos(1)=j
        exit   do1
      end if
   end do
end do do1
end

subroutine level3
use modsokoban
      level=3
      levi=11
      levj=19
      mappy=-1
      mappy(0,0:levj-1) =(/0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0/)
      mappy(1,0:levj-1) =(/0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0/)
      mappy(2,0:levj-1) =(/0,0,0,0,1,3,0,0,1,0,0,0,0,0,0,0,0,0,0/)
      mappy(3,0:levj-1) =(/0,0,1,1,1,0,0,3,1,1,0,0,0,0,0,0,0,0,0/)
      mappy(4,0:levj-1) =(/0,0,1,0,0,3,0,3,0,1,0,0,0,0,0,0,0,0,0/)
      mappy(5,0:levj-1) =(/1,1,1,0,1,0,1,1,0,1,0,0,0,1,1,1,1,1,1/)
      mappy(6,0:levj-1) =(/1,0,0,0,1,0,1,1,0,1,1,1,1,1,0,0,2,2,1/)
      mappy(7,0:levj-1) =(/1,0,3,0,0,3,0,0,0,0,0,0,0,0,0,0,2,2,1/)
      mappy(8,0:levj-1) =(/1,1,1,1,1,0,1,1,1,0,1,4,1,1,0,0,2,2,1/)
      mappy(9,0:levj-1) =(/0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,1/)
      mappy(10,0:levj-1)=(/0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0/)
      call playerposition
end

subroutine level1
use modsokoban
      level=1
      levi=6
      levj=8
      mappy=-1
     mappy(0,0:levj-1) =(/1,1,1,1,0,0,0,0/)
     mappy(1,0:levj-1) =(/1,0,0,1,1,1,1,1/)
     mappy(2,0:levj-1) =(/1,0,2,0,0,3,0,1/)
     mappy(3,0:levj-1) =(/1,0,3,0,0,2,4,1/)
     mappy(4,0:levj-1) =(/1,1,1,0,0,1,1,1/)
     mappy(5,0:levj-1) =(/0,0,1,1,1,1,0,0/)
      call playerposition

end

subroutine level2
use modsokoban
      level=2
      levi=6
      levj=8
      mappy=-1
     mappy(0,0:levj-1) =(/1,1,1,1,1,1,1,0/)
     mappy(1,0:levj-1) =(/1,2,0,0,0,0,0,1/)
     mappy(2,0:levj-1) =(/1,0,0,0,0,3,0,1/)
     mappy(3,0:levj-1) =(/1,0,0,0,0,0,4,1/)
     mappy(4,0:levj-1) =(/1,1,1,0,0,1,1,1/)
     mappy(5,0:levj-1) =(/0,0,1,1,1,1,0,0/)
      call playerposition

end

subroutine possible_move(y,x)
use modsokoban
   integer tile,far_tile,x,y,ox,oy
   logical can_move,solved

        tile = mappy(playerpos(0)+y,playerpos(1)+x)
        far_tile = mappy(playerpos(0)+2*y,playerpos(1)+2*x)
     		print *, 'tile' ,tile,far_tile,playerpos
        can_move = .false.
     		solved = .true.

     		if(tile==0 .or. tile==2) then
                    can_move = .true.
                    solved = .false.
        else
          if((tile==3 .or. tile==5) .and. (far_tile==0 .or. far_tile==2)) then
            mappy(playerpos(0)+y,playerpos(1)+x)=mappy(playerpos(0)+y,playerpos(1)+x)-3
            mappy(playerpos(0)+2*y,playerpos(1)+2*x)=mappy(playerpos(0)+2*y,playerpos(1)+2*x)+3
            do1:  do i=0,levi-1
               do j=0,levj-1
                  if(mappy(i,j)==2) then
                     solved = .false.
                     exit do1
                  end if
               end do
             end do do1

             if (solved) then
               print *,"SOLVED"
             end if
             can_move = .true.
          end if
        end if

        if(can_move)then
          ox=playerpos(0)
          oy=playerpos(1)
          mappy(ox,oy)=mappy(ox,oy)-4
          playerpos(0)=playerpos(0)+y
          playerpos(1)=playerpos(1)+x
          mappy(playerpos(0),playerpos(1))=mappy(playerpos(0),playerpos(1))+4
     		end if

end subroutine

      subroutine check(a)
      character*1 a

      select case(a)
       case('a')  ! l left
    				call possible_move(0,-1)
       case('w')  ! u up
     				call possible_move(-1,0)
       case('d')  ! r right
     				call possible_move(0,1)
       case('s') ! d down
     				call possible_move(1,0)
       case('q')
          level=level+1
          stop
       end select

       end subroutine

       subroutine showold
       use modsokoban

       write(*,'(8i3)') (((mappy(i,j)),j=0,levj),i=0,levi)

       end

       subroutine show
       use modsokoban

       do i=0,levi-1
         do j=0,levj-1
           select case(mappy(i,j))

            case(0)
              write(*,'(a2)',advance='no') "  "

            case(1)
              write(*,'(a2)',advance='no') "##"

            case(2)
              write(*,'(a2)',advance='no') "GG"

            case(3)
              write(*,'(a2)',advance='no') "%%"

            case(4)
              write(*,'(a2)',advance='no') "PP"

            case(5)
              write(*,'(a2)',advance='no') "G%"

            case(6)
              write(*,'(a2)',advance='no') "P%"
           end select
         end do
         write(*,*)

       end do
       end

       program tets
       use modsokoban
       character*1 a
       call init
       do
        call show
        read *,a
        call check(a)
       end do
       end
