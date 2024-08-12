module class_Product
    implicit none

    type :: Product
        character(len = 100) :: name
        integer :: amount
        real :: price
        character(len = 100) :: location

    contains

        procedure :: getName => getName
        procedure :: setName
        procedure :: getAmount => getAmount
        procedure :: setAmount
        procedure :: getPrice => getPrice
        procedure :: setPrice
        procedure :: getLocation => getLocation
        procedure :: setLocation
        procedure :: printDates

    end type Product

    contains

        ! IMPRIME LOS 
        subroutine printDates(this)
            class(Product), intent(in) :: this
            print *, this%name, this%amount, this%price, this%location
        end subroutine printDates

        !get and set fot name
        function getName(this) result(name)
            class(Product), intent(in) :: this
            character(len = 100) :: name
            name = this%name
        end function getName

        subroutine setName(this,  name)
            class(Product), intent(inout) :: this
            character(len = 100) :: name
            this%name = name
        end subroutine setName

        !get and set for amount
        function getAmount(this) result(amount)
            class(Product), intent(in) :: this
            integer :: amount
            amount = this%amount
        end function getAmount

        subroutine setAmount(this, amount)
            class(Product), intent(inout) :: this
            integer :: amount
            this%amount = amount

        end subroutine setAmount

        function getPrice(this) result(price)
            class(Product), intent(in) :: this
            real :: price
            price = this%price
        end function getPrice

        subroutine setPrice(this, price)
            class(Product), intent(inout) :: this
            real :: price
            this%price = price

        end subroutine setPrice

        function getLocation(this) result(location)
            class(Product), intent(in) :: this
            character(len = 100) :: location
            location = this%location
        end function getLocation

        subroutine setLocation(this, location)
            class(Product), intent(inout) :: this
            character(len = 100) :: location
            this%location = location

        end subroutine setLocation
    
end module class_Product