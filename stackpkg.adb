package body stackPkg is

    -- Returns true is the top value is equal to zero, otherwise returns false.
    function isEmpty (s : stack) return boolean is
    begin
        return s.top = 0;
    end isEmpty;

    -- Returns true if the top value is equal to the max size of the stack,
    -- otherwise returns false.
    function isFull (s : Stack) return boolean is
    begin
        return s.top = size;
    end isFull;

    -- Increases the top value by one if the stack is not full, and then assignes the
    -- elements of the array at the new top index the value of item.
    procedure push (item : itemType; s : in out Stack) is
    begin
        if isFull(s) then
            raise stack_full;
        else
            s.top := s.top + 1;
            s.elements(s.top) := item;
        end if;
    end push;

    -- Decreases the value of the top variable by one if the stack is not empty, otherwise
    -- throws an exception
    procedure pop (s : in out stack) is
    begin
        if isEmpty(s) then
            raise stack_empty;
        else
            s.top := s.top - 1;
        end if;
    end pop;

    -- Returns the top element from the stack s.
    function top (s : Stack) return itemType is
    begin
        if isEmpty(s) then
            raise stack_empty;
        else
            return s.elements(s.top);
        end if;
    end top;

end stackpkg;
