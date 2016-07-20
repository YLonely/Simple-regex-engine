#include "stdafx.h"

enum type
{
	point = -9,
	l_parentheses,
	r_parentheses,
	add,
	mul,
	ques,
	l_bracket,
	r_bracket
};

int type_match(int ch)
{
	switch (ch)
	{
	case 'n':
		return 10;
	case 't':
		return 9;
	case '.':
		return point;
	case '(':
		return l_bracket;
	case ')':
		return r_bracket;
	case '+':
		return add;
	case '*':
		return mul;
	case '?':
		return ques;
	default:
		return 0;
	}
}

/*
Handle the params in the brackets [ ].
*/

void bracket_feature_handler(char* begin, char* end, char** new_regex)
{
	char *begin_p, *end_p;
	begin_p = begin;
	end_p = end;


	char begin_char, end_char, next_char;
	while (begin_p != end_p)
	{
		if (*begin_p == '-')
		{
			printf("Regex error.\n");
			getchar();
			exit(1);
		} else
		{
			begin_char = *begin_p;
			next_char = *(begin_p + 1);
			if (next_char == '-')
			{
				if (*(begin_p - 1) != '[')
				{
					*((*new_regex)++) = '|';
				}
				end_char = *(begin_p + 2);
				for (int i = begin_char; i < end_char; i++)
				{
					*((*new_regex)++) = i;
					*((*new_regex)++) = '|';
				}
				*((*new_regex)++) = end_char;
				begin_p = begin_p + 2; //step over the x-y param.
			} else
			{
				if (*(begin_p - 1) != '[')
				{
					*((*new_regex)++) = '|';
				}
				*((*new_regex)++) = begin_char;
			}
		}
		begin_p++;
	}
}


/*
Add features of '\',features of [x-y].
*/
char* regex_handler(char* regex)
{
	char* new_regex = (char*)malloc(1024 * sizeof(char));
	char* new_regex_p = new_regex;

	char* string_p = regex;
	char* look_ahead_p = string_p;
	char new_type;
	while (*string_p)
	{
		switch (*string_p)
		{
		case '\\':
			if (new_type = type_match(*(++string_p)))
			{
				*(new_regex++) = new_type;
			} else
			{
				*(new_regex++) = *string_p;
			}
			break;
		case '[':
			*(new_regex++) = '(';
			look_ahead_p = string_p;
			for (; *look_ahead_p != 0 && *look_ahead_p != ']'; look_ahead_p++);
			if (*look_ahead_p == 0)
			{
				printf("Regex error\n");
				getchar();
				exit(0);
			}
			bracket_feature_handler(string_p + 1, look_ahead_p, &new_regex);
			string_p = look_ahead_p;
			*(new_regex++) = ')';
			break;
		default:
			*(new_regex++) = *string_p;
			break;
		}
		string_p++;
	}
	*new_regex = 0;
	return new_regex_p;
}


/*
Convert infix regex regex to postfix notation
return a string pointer of the postfix regex;
*/
char* re2post(char *regex)
{
	/*
	alt_num: The number of logic or--'|'.
	atom_num: The number of atoms in the final result.
	*/
	int alt_num = 0, atom_num = 0, regex_length;
	char *post_fix = NULL;
	struct
	{
		int _alt_num;
		int _atom_num;
	}levels_arr[100], *level_pt;
	level_pt = levels_arr;
	if ((post_fix = (char*)malloc(1024 * sizeof(char))) == NULL)
	{
		printf("Malloc failed\n");
		return NULL;
	}

	char *post = post_fix;
	char *new_regex = regex_handler(regex);

	for (; *new_regex; new_regex++)
	{
		switch (*new_regex)
		{
		default:
			if (atom_num > 1)
			{
				atom_num--;
				*post_fix++ = -1;
			}
			*post_fix++ = *new_regex;
			atom_num++;
			break;
		case '(':
			if (atom_num > 1)
			{
				atom_num--;
				*post_fix++ = -1;
			}
			if (alt_num >= 100)
			{
				printf("regex level is greater than max level\n");
				return NULL;
			}
			level_pt->_alt_num = alt_num;
			level_pt->_atom_num = atom_num;
			level_pt++;
			atom_num = 0;
			alt_num = 0;
			break;
		case ')':
			if (level_pt == levels_arr)
			{
				printf("There is a ')' without '('\n");
				return NULL;
			}
			if (atom_num == 0)
			{
				printf("There is no object before ')'\n");
				return NULL;
			}
			while (--atom_num > 0)
				*post_fix++ = -1;
			while (alt_num-- > 0)
				*post_fix++ = '|';
			level_pt--;
			atom_num = level_pt->_atom_num;
			alt_num = level_pt->_alt_num;
			atom_num++;
			break;
		case '+':
		case '*':
		case '?':
			if (atom_num == 0)
			{
				printf("There is no object ahead\n");
				return NULL;
			}
			*post_fix++ = *new_regex;
			break;
		case '|':
			if (atom_num == 0)
			{
				printf("There is no object ahead\n");
				return NULL;
			}
			while (--atom_num > 0)
				*post_fix++ = -1;
			alt_num++;
			break;
		}
	}
	if (level_pt != levels_arr)
	{
		printf("Wrong form of regex\n");
		return NULL;
	}
	while (--atom_num > 0)
		*post_fix++ = -1;
	while (alt_num-- > 0)
		*post_fix++ = '|';
	*post_fix = 0;
	return post;
}

/*
Represents an NFA state plus zero or one or two arrows exiting.
if c== Match, no arrows out;matching state.
if c== Split, unlabeled arrows to out and out1.
if c<256, labeled arrow with character c to out.
*/
#define MATCH 257
#define SPLIT 256
typedef struct _state
{
	int c;
	_state ****out1 ;
	_state ****out2 ;
}state;

state match_state = { MATCH };
int g_state_num = 0;

/*
 Allocate and initialize state
*/
state* state_ini(int c, state *out1, state *out2)
{
	state *s;
	g_state_num++;
	s = (state*)malloc(sizeof(_state));
	//s->lastlist = 0;
	s->c = c;
	s->out1 = (state****)malloc(sizeof(state***));
	s->out2 = (state****)malloc(sizeof(state***));
	*(s->out1) = (state***)malloc(sizeof(state**));
	*(s->out2) = (state***)malloc(sizeof(state**));
	*(*(s->out1)) = (state**)malloc(sizeof(state*));
	*(*(s->out2)) = (state**)malloc(sizeof(state*));
	*(*(*(s->out1))) = out1;
	*(*(*(s->out2))) = out2;
	//s->id = id;
	return s;
}

/*
 A partially built NFA without the matching state filled in.
 Frag.start points at the start state.
 Frag.out is a list of places that need to be set to the next state for this fragment.
*/


typedef struct _frag
{
	state *start;
	state ****out;
}frag;

frag frag_ini(state *start, state ****out1)
{
	frag n = { start,out1 };
	return n;
}



/*
 Patch the state at the out of the start state.
*/
void patch(state ****out, state *start)
{
	*(*(*out)) = start;
}

/*
 Join the two state pointer,returning the combination
*/
state**** append(state ****s1, state ****s2)
{
	*(*(s2)) = *(*(s1));
	return s1;
}

/*
 Convert postfix regex to NFA
 Return start state
*/
state* post2nfa(char* postfix)
{
	char *p = NULL;
	frag nfa_stack[100], *stack_p = NULL, e1, e2, e;
	state *s = NULL;

	if (postfix == NULL)
	{
		printf("Postfix is empty\n");
		return NULL;
	}

#define push(s) (*(stack_p++) = s)
#define pop() *(--stack_p)

	stack_p = nfa_stack;
	for (p = postfix; *p; p++)
	{
		switch (*p)
		{
		default:
			s = state_ini(*p, NULL, NULL);
			_frag m = frag_ini(s, s->out1);
			push(m);
			break;
		case -1:
			e2 = pop();
			e1 = pop();
			patch(e1.out, e2.start);
			push(frag_ini(e1.start, e2.out));
			break;
		case '|':
			e2 = pop();
			e1 = pop();
			s = state_ini(SPLIT, e1.start, e2.start);
			*e1.out = *e2.out;
			push(frag_ini(s, append(e1.out, e2.out)));
			break;
		case '?':
			e = pop();
			s = state_ini(SPLIT, e.start, NULL);
			*(e.out) = *(s->out2);
			push(frag_ini(s, e.out));
			break;
		case '*':
			e = pop();
			s = state_ini(SPLIT, e.start, NULL);
			patch(e.out, s);
			push(frag_ini(s, s->out2));
			break;
		case '+':
			e = pop();
			s = state_ini(SPLIT, e.start, NULL);
			patch(e.out, s);
			push(frag_ini(e.start, s->out2));
			break;
		}
	}
	e = pop();
	if (stack_p != nfa_stack)
	{
		printf("Error\n");
		return NULL;
	}
	//printf("%x %x", &(e.start->out1->out1), &(e.start->out2->out1));
	patch(e.out, &match_state);
	return e.start;
#undef pop()
#undef push(s)
}

/*
 A struct that stored the states that nfa is in,
 and the nfa will be in.
*/
typedef struct _list
{
	state **s;
	int states_num;
}list;

void add_state(list*, state*);
void advance(list*, int, list*);

void start_list(state *start, list *l)
{
	l->states_num = 0;
	add_state(l, start);
}

/*
 Add s to l, following unlabeled arrows.
*/
void add_state(list *l, state *s)
{
	if (s == NULL)
		return;
	//s->lastlist = list_id;
	if (s->c == SPLIT)
	{
		add_state(l, *(*(*(s->out1))));
		add_state(l, *(*(*(s->out2))));
		return;
	}
	l->s[l->states_num++] = s;
}

/*
 Step the nfa from the states in clist
 past the character c to create next nfa state set nlist.
*/
void advance(list *clist, int c, list *nlist)
{
	state *temp;
	//list_id++;
	nlist->states_num = 0;
	for (int i = 0; i < clist->states_num; i++)
	{
		temp = clist->s[i];
		if (temp->c == c || temp->c == '.')
			add_state(nlist, *(*(*(temp->out1))));
	}
}

/*
 Run nfa to determine whether it matches s.
*/
int match(state *start, char *s)
{
	int c;
	int ch;
	list current_list, next_list, temp;
	current_list.s = (state**)malloc(g_state_num*sizeof(state*));
	next_list.s = (state**)malloc(g_state_num*sizeof(state*));
	start_list(start, &current_list);
	for (; *s; s++)
	{
		c = *s;
		if (ch = type_match(c))
		{
			advance(&current_list, ch, &next_list);
		} else
			advance(&current_list, c, &next_list);
		advance(&current_list, c, &next_list);
		temp = current_list;
		current_list = next_list;
		next_list = temp;
	}
	g_state_num = 0;
	for (int i = 0; i < current_list.states_num; i++)
	{
		if (current_list.s[i] == &match_state)
		{
			free(current_list.s);
			free(next_list.s);
			return 1;
		}
	}
	free(current_list.s);
	free(next_list.s);
	return 0;
}

/*
 The toppest driver of the regex matching engine.
*/
int ismatch(char *regex, char *string)
{
	if (regex == NULL || string == NULL || !strcmp(regex, "") || !strcmp(string, ""))
	{
		printf("Wrong params\n");
		return 0;
	}
	char *post = re2post(regex);
	state *start = post2nfa(post);
	return match(start, string);
}