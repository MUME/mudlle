// Changes to MUME symbol names
// Ideas:
//  "type name" starts operations strongly linked to type
//  (eg room_name is strongly linked, find_the_path isn't)


// Issues:
//   gain_x_y, change_x_y, x_y_gain!, x_y_change! ??
//   should gain_x_y be a primitive (set! better ?)

//   what should operations that return an x be called ?
//   (eg: get_player_number, get_skill_number)
//   maybe? lookup_player, lookup_skill, etc
//   (ie: lookup_x takes an identifier for x and returns the corresponding
//    x, or false if not found)

// rooms & zones
change_room_light = gain_room_light;
set_exit_flags! = exit_flags_set!;
room_set = room_set!;
get_zone_alignment = zone_alignment;
change_zone_alignment = gain_zone_alignment;

// Misc
override_command = override;
get_skill_info = skill_info;
get_skill_number = lookup_skill;


// Objects
obj_set = obj_set!;

// Problems:
//  -- prefix problems: obj / object
//  -- find_xxx: xxx should be obj, maybe lookup_xxx (but returns several...)
//  -- get_obj_x, set_obj_x instead of char_x, char_x_set!
//  -- change/gain/set
//  -- model vs instance
//  -- what about instance access/modification vs "world" modification ?
//     (eg set_obj_val vs obj_hide), conceptually different ...

// Characters
char_set = char_set!;

// Problems:
//  -- prefix problems: char / character
//  -- find_xxx: xxx should be char, maybe lookup_xxx (but returns several...)
//  -- is_xxx? (should be just xxx? or char_xxx?)
//  -- get_char_x, set_char_x instead of char_x, char_x_set!
//  -- change/gain/set
//  -- model vs instance

// ideas:
//   find_player_x -> find_char_x
//   remove get, move set_x to x_set!
//   is_x? -> char_x?
//   player_x: accesses player's model
//   mobile_x: accesses mobile's model

