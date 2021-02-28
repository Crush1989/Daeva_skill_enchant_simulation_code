      program daevaskillenchant

      implicit none

      integer :: start_level,num_sim,level,i,num_books, &
                 use_de,num_de        
      double precision :: rand_number,avg,tot_de,rate
! declare array to store number of times a certain number of books was
! needed to get the skill to 15. Picking 500 as the max amount since
! that is extremely unlikely you'll ever be this unlucky!
      integer, dimension(500) :: books_needed
      integer :: enchant_no_essence,enchant_with_essence
      double precision :: get_rate



      print*, "Enter the starting level of your skill and hit enter: "
      read(*,*) start_level
      print*, "Enter the level you will start using daeva essences. &
            If you won't use any then enter 15:"
      read(*,*) use_de
      print*, "How many simulations do you want to do? I suggest &
            doing between 1,000 and 1,000,000: "
      read(*,*) num_sim

      open(unit = 7, file = 'sim_results.txt',status='unknown')
      
      call random_seed()


        books_needed(:) = 0
        avg = 0.0
        tot_de = 0

      
      do i = 1,num_sim
        level = start_level
        num_books = 0
        num_de = 0
        do while (level .lt. 15)
        
          call random_number(rand_number)

! Found these rates on the forum (from KR), not sure if they are correct.
          rate = get_rate(level)

! 10 is a safe level, never use a daeva essence here!       
          if (level .eq. 10) then
! use enchant_with_essence function but don't increment number 
! of daeva essences used.
             level = enchant_with_essence(level,rand_number,rate)
          else if ( level .lt. use_de ) then
             level = enchant_no_essence(level,rand_number,rate)
          else if ( level .ge. use_de ) then
             level = enchant_with_essence(level,rand_number,rate)
             num_de = num_de + 1
          endif
          num_books = num_books + 1
        enddo
        books_needed(num_books) = books_needed(num_books) + 1
        tot_de = tot_de + num_de
      enddo

      tot_de = tot_de / num_sim
      print*, 'average number of daeva essensce needed: ', tot_de
      do i = 1,500
! Write to results file, the first columns is the number of books used
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


      end function get_rate

