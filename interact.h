#ifndef INTERACT_H
#define INTERACT_H

void mudlle_init(void);
void mudlle_load_all(void);
void mudlle_init_player(struct char_data *ch);
void mudlle_cleanup(void);
void do_mudlle(struct char_data *ch, char *argument);
void do_react_mudlle(struct char_data *ch, char *argument);
void mudlle_act(void *data, struct descriptor_data *d, char *command);
void mudlle_prompt(void *data, struct descriptor_data *d, char *prompt, int prompt_size);
int load_file(char *name, char *nicename, int seclev, int reload);
int catch_load_file(char *name, char *nicename, int seclev, int reload);

#endif
