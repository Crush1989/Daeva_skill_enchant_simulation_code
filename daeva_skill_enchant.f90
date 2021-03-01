      program daevaskillenchant
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
! This program calculates the average number of daeva skill books and !
! daeva essences needed to enchant a skill to +15 subject to the      !  
! user's input parameters that they will be prompted to enter at the  !
! start of the program.                                               !
!                                                                     ! 
! Written by: Crush 2/28/2021                                         !
!                                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!              
      implicit none

      integer :: start_level,num_sim,level,i,num_books_used, &
                 use_de_lvl,num_de_used        
      double precision :: rand_number,avg,tot_de,rate
! declare array to store number of times a certain number of books was
! needed to get the skill to 15. Picking 500 as the max amount since
! that is extremely unlikely you'll ever be this unlucky!
      integer, dimension(500) :: books_needed
      integer :: enchant_no_essence,enchant_with_essence
      double precision :: get_rate

      call get_user_input(start_level,use_de_lvl,num_sim)

      open(unit = 7, file = 'sim_results.txt',status='unknown')
      
      call random_seed()

      books_needed(:) = 0
      avg = 0.0
      tot_de = 0

      do i = 1,num_sim
        level = start_level
        num_books_used = 0
        num_de_used = 0
        do while (level .lt. 15)
          call random_number(rand_number)
! Get the rate appropriate for the level. Found these rates on 
! the forum (from KR), not sure if they are correct.
          rate = get_rate(level)
! Enchant the skill and use daeva essence when the user wanted to.
          call enchant(level,rand_number,rate,use_de_lvl,num_de_used)
          num_books_used = num_books_used + 1
        enddo
        books_needed(num_books_used) = books_needed(num_books_used) + 1
        tot_de = tot_de + num_de_used
      enddo
! Calculate average daeva essences needed.
      tot_de = tot_de / num_sim 

      print*, 'average number of daeva essensce needed: ', tot_de
      do i = 1,500
! Write to the results file, the first columns is the number of books used
! to +15 a skill and the second column is the number of times this
! amount of books was needed.
            write(7,*) i,books_needed(i)
            avg = avg + i*books_needed(i)
      end do
      avg = avg / num_sim
      print*, 'average number of books used: ', avg


      end program daevaskillenchant 




! Function that enchants daeva skills with no daeva essences
      function enchant_no_essence (ol,rn,r)

      implicit none
      
      integer :: ol,enchant_no_essence
      double precision :: rn,r

      if ( rn .gt. 1-r ) then
              enchant_no_essence = ol + 1
      else
              enchant_no_essence = ol - 1
      endif
      return
      end function enchant_no_essence

! Function that enchants daeva skills with daeva essences
      function enchant_with_essence (ol,rn,r)

      implicit none

      integer :: ol,enchant_with_essence
      double precision :: rn,r

      if ( rn .gt. 1-r ) then
              enchant_with_essence = ol + 1
      else
              enchant_with_essence = ol 
      endif
      return
      end function enchant_with_essence

! Function that gets the correct enchant rate for a given level
      function get_rate (l)

      implicit none

      integer :: l
      double precision :: get_rate

      ! Found these rates on the forum (from KR), not sure if they are correct.
        if ( l .eq. 0 ) then
              get_rate = 1.0
        else if ( l .gt. 0  .and. l .lt. 5 ) then
              get_rate = 0.9
        else if ( l .ge. 5  .and. l .lt. 10) then
              get_rate = 0.7
        else if ( l .ge. 10 .and. l .lt. 15) then
              get_rate = 0.6
        endif
      return
      end function get_rate

! This subroutine enchants the skill with the proper rates and with or
! without daeva essences depending on the user's input.      
      subroutine enchant (l,rn,r,udel,ndeu)

      implicit none

      integer :: l,ndeu,udel
      double precision :: rn,r
      integer :: enchant_no_essence,enchant_with_essence 

! 10 is a safe level, never use a daeva essence here!       
          if (l .eq. 10) then
! use enchant_with_essence function but don't increment number 
! of daeva essences used.
             l = enchant_with_essence(l,rn,r)
          else if ( l .lt. udel ) then
             l = enchant_no_essence(l,rn,r)
          else if ( l .ge. udel ) then
             l = enchant_with_essence(l,rn,r)
             ndeu = ndeu + 1
          endif
      return
      end
! This subroutine gets the user's input parameters
      subroutine get_user_input (sl,udel,ns)

      implicit none

      integer :: sl,udel,ns

      print*, "Enter the starting level of your skill and hit enter: "
      read(*,*) sl
      print*, "Enter the level you will start using daeva essences. &
            If you won't use any then enter 15:"
      read(*,*) udel
      print*, "How many simulations do you want to do? I suggest &
            doing between 1,000 and 1,000,000: "
      read(*,*) ns
      return
      end
 
