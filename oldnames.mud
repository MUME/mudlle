set_car = set_car!;
set_cdr = set_cdr!;
string_set = string_set!;
string_fill = string_fill!;
table_set = table_set!;
symbol_set = symbol_set!;
vector_fill = vector_fill!;
vector_set = vector_set!;
set = set!;

// closure? has changed in meaning.
foreach = lforeach;
mapcar = lmap;

filter_list = fn "l1 fn -> l2. Returns l1 filtered by function fn" (l, f)
  lfilter(f, l);

find_first = fn "l fn -> x. Returns first element x of l for which fn(x) is true, false if none found" (l, f) lexists?(f, l);

mapvec = vmap;
eq? = fn (x1, x2) x1 == x2;
reverse_list = lreverse;
list_member? = memq;

list_append = fn "l1 l2 -> l3. l3 is l2 appended to l1. Anc.standard" 
   (l1, l2) [
   |res|
   res = list_copy (l1);
   if (not pair? (res)) res = list_copy (l2)
   else set_cdr (last_pair (res), list_copy (l2));
   res;
];

list_append! = list_append_set = lappend!;
list_first_n_set = list_first_n!;
filter_list! = filter_list_set = fn (l, f) lfilter!(f, l);
list_delete! = list_delete_set = ldelete!;

merge_words = fn "l -> s. Reverses split_words" (l) concat_words(l, " ");

find_index = string_index;
find_rindex = string_rindex;
bitset2list = bitset_to_list;
vector_find = vexists?;

// Hmm, maybe should be kept
list_length = llength;
list_copy = lcopy;


