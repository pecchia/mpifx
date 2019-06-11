include(mpifx_common.m4)

dnl ************************************************************************
dnl *** mpifx_scatterv
dnl ************************************************************************

define(`_subroutine_mpifx_scatterv_dr0',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: send/recv buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: send/recv buffer rank (1, 2, etc.)
dnl $5: corresponding MPI type
dnl
!> scatters object of variable length from one process (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for scatterving.
!! \param recv  Received data on receive node (undefined on other nodes)
!! \param sendcounts Counts of sent data from each process
!! \param displs Entry i specifies where to take data to send to rank i 
!!               (default: computed from sendcounts assuming order with rank)
!! \param root  Root process for the result (default: mycomm%masterrank)
!! \param error  Error code on exit.
!!
subroutine mpifx_scatterv_$1(mycomm, send, sendcounts, displs, recv, root, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$3
  $2, intent(out) :: recv$3
  integer, intent(in) :: sendcounts(:)
  integer, intent(in), optional :: displs(:)
  integer, intent(in), optional :: root
  integer, intent(out), optional :: error

  integer :: root0, error0, ii
  integer, allocatable :: displs0(:)

  _assert(.not. mycomm%master .or. size(send) == size(recv) * mycomm%size)
  _assert(.not. mycomm%master .or. &
      & size(send, dim=$4) == size(recv, dim=$4) * mycomm%size)

  _handle_inoptflag(root0, root, mycomm%masterrank)

  if (mycomm%rank == root0) then
    _assert(size(send) == sum(sendcounts))
    allocate(displs0(mycomm%size))
    if (present(displs)) then
      _assert(size(displs) == mycomm%size)
      displs0 = displs
    else
      displs0(1) = 0
      do ii = 2, mycomm%size
        displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
      end do
    end if
  end if
  
    call mpi_scatterv(send, size(send), sendcount, displs0, &
      & $5, recv, size(recv), $5, root0, mycomm%id, error0)

  call handle_errorflag(error0, "MPI_SCATTERV in mpifx_scatter_$1", error)
    
end subroutine mpifx_scatterv_$1
')


define(`_subroutine_mpifx_scatterv_dr1',`dnl
dnl
dnl $1: subroutine suffix
dnl $2: send/recv buffer type
dnl $3: recv buffer rank specifier ("", (:), (:,:), etc.)
dnl $4: recv buffer size (1 or size(recv))
dnl $5: send buffer rank specifier ((:), (:,:), etc.)
dnl $6: send buffer rank (1, 2, etc.)
dnl $7: corresponding MPI type
dnl
!> Scatter results on one process (type $1).
!!
!! \param mycomm  MPI communicator.
!! \param send  Quantity to be sent for scatterving.
!! \param recv  Received data on receive node (indefined on other nodes)
!! \param sendcounts Counts of received data from each process
!! \param displs Entry i specifies where data are placed in rank i 
!!               (default: computed from recvcounts assuming order with rank)
!! \param root  Root process for the result (default: mycomm%masterrank)
!! \param error  Error code on exit.
!!
subroutine mpifx_scatterv_$1(mycomm, send, sendcounts, displs, recv, root, error)
  type(mpifx_comm), intent(in) :: mycomm
  $2, intent(in) :: send$5
  $2, intent(out) :: recv$3
  integer, intent(in) :: sendcounts(:)
  integer, intent(in), optional :: displs(:)
  integer, intent(in), optional :: root
  integer, intent(out), optional :: error

  integer :: ii, root0, error0
  integer, allocatable :: displs0(:)

  _handle_inoptflag(root0, root, mycomm%masterrank)

  if (mycomm%rank == root0) then
    _assert(size(send) == sum(sendcounts))
    _assert(size(send, dim=$6) == mycomm%size)
    allocate(displs0(mycomm%size))
    if (present(displs)) then
      _assert(size(displs) == mycomm%size)
      displs0 = displs
    else
      displs0(1) = 0
      do ii = 2, mycomm%size
        displs0(ii) = displs0(ii-1) + sendcounts(ii-1)
      end do
    end if
  end if

  call mpi_scatterv(send, sendcounts, displs0, $7, recv, $4, &
      & $7, root0, mycomm%id, error0)

  call handle_errorflag(error0, "MPI_SCATTERV in mpifx_scatterv_$1", error)

end subroutine mpifx_scatterv_$1

')
