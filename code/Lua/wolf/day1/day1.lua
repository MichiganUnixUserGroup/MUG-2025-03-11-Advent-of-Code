function read_the_two_lists ()
    -- Lua only has one data-structure: the table.  Here, I make two that I will use as lists.
    -- To be a list, I just have to use numerical keys.  Note that in Lua, lists are 1-based.
    -- It's possible to make a list with 'holes'.  Don't do that.  It messes everything up.
    local left = {}
    local right = {}

    while true do
        -- We haven't pointed io at a file, so io.read gets the next line from stdin.  The args
        -- "n", "n" mean to read two numbers.  Lua can return multiple values from a function,
        -- and if the left-hand side of an assignment supplies multiple names, you can collect
        -- them all.
        left_val, right_val = io.read("n", "n")
        if not left_val or not right_val then
            -- Only nil and false are falsey.  io.read, will return a nil at the end of the file.
            -- Therefore, we will stop reading and stop appending to the lists.  A consequence of
            -- this is that the lists are guaranteed to be the same length.
            break
        end

        -- # is the length operator; lists are 1-based, therefore, #left is the last occupied
        -- slot in left (if we're using left as a list, that is).  Assigning a value into an
        -- unoccupied slot, #left + 1, grows the list.  You can do the same thing with .insert
        -- but this is clearer.
        left[#left + 1] = left_val
        right[#right + 1] = right_val
    end

    return left, right
end

function distance_between_the_two_lists (left, right)
    -- table.sort sorts in place
    table.sort(left)
    table.sort(right)

    -- no comprehensions or iterator chains in Lua (though there _are_ iterators)
    local total_distance = 0
    for k = 1, #left do
        -- no += in Lua
        total_distance = total_distance + math.abs(left[k] - right[k])
    end

    return total_distance
end

function similarity_score (left, right)
    -- Step 1: build a "bag" from the right list.  That is, a dictionary where
    -- the keys are the elements of right and the values are the number of times
    -- that value appears in right
    local right_bag = {}
    for i, v in ipairs(right) do
        -- if v isn't in right_bag, right_bag[v] returns nil, which is falsey, you get 0
        right_bag[v] = (right_bag[v] or 0) + 1
    end

    local total_similarity = 0
    for i, v in ipairs(left) do
        total_similarity = total_similarity + v * (right_bag[v] or 0)
    end

    return total_similarity
end

function day1_from_stdin()
    local left, right = read_the_two_lists()

    local distance = distance_between_the_two_lists(left, right)
    print("Day 1, part 1: the distance between the two lists is " .. distance .. ".")

    local similarity = similarity_score(left, right)
    print("Day 1, part 2: the similarity score for the two lists is " .. similarity .. ".")
end

function test_day1()
    local left = {3, 4, 2, 1, 3, 3}
    local right = {4, 3, 5, 3, 9, 3}

    local distance = distance_between_the_two_lists(left, right)
    print("Day 1, part 1 test: the distance between the two lists is " .. distance .. " should be 11.")

    local similarity = similarity_score(left, right)
    print("Day 1, part 2 test: the similarity score for the two lists is " .. similarity .. " should be 31.")
end

-- Note: by default, I run "production" mode.  TODO: make it possible to run test_day1.
day1_from_stdin()
