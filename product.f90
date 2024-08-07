module class_Product
    implicit none

    type :: Product
        character(:), allocatable :: name
        integer :: amount
        real :: price
        character(:), allocatable :: location

    contains

        procedure :: getName => getName
        procedure :: setName
        procedure :: getAmount => getAmount
        procedure :: getPrice => getPrice
        procedure :: getLocation => getLocation
        procedure :: printDates

    end type Product

    contains

        subroutine printDates(this)
            class(Product), intent(in) :: this
            print *, this%name, this%amount, this%price, this%location
        end subroutine printDates

        function getName(this) result(name)
            class(Product), intent(in) :: this
            character(:), allocatable :: name
            name = this%name
        end function getName

        subroutine setName(this,  name)
            class(Product), intent(inout) :: this
            character(:), allocatable :: name
            this%name = name
        end subroutine setName

        function getAmount(this) result(amount)
            class(Product), intent(in) :: this
            integer :: amount
            amount = this%amount
        end function getAmount

        function getPrice(this) result(price)
            class(Product), intent(in) :: this
            real :: price
            price = this%price
        end function getPrice

        function getLocation(this) result(location)
            class(Product), intent(in) :: this
            character(:), allocatable :: location
            location = this%location
        end function getLocation
    
end module class_Product