#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define ONE_MILLION 1000000
#define SCORES_BUFFER_SIZE (30 * ONE_MILLION)

typedef struct {
  int score_count, ticks;
  int *scores;
  int *elves;
} State;

State *mkstate() {
  State *s = malloc(sizeof(State));
  s->ticks = 0;
  s->score_count = 2;
  s->scores = malloc(SCORES_BUFFER_SIZE * sizeof(int));
  s->scores[0] = 3;
  s->scores[1] = 7;
  s->elves = malloc(2 * sizeof(int));
  s->elves[0] = 0;
  s->elves[1] = 1;
  return s;
}

void tick(State *state) {
  state->ticks += 1;
  int combined = state->scores[state->elves[0]] + state->scores[state->elves[1]];

  // if combined is 2 digits, store the first one as a new recipe
  if (combined >= 10) {
    state->scores[state->score_count] = 1;
    state->score_count += 1;
    combined = combined % 10;
  }

  // store the last/only digit of combined as a new recipe
  state->scores[state->score_count] = combined;
  state->score_count += 1;

  // move the elves
  for (int i = 0; i < 2; i++) {
    int cur_recipe_idx = state->elves[i];
    state->elves[i] = (cur_recipe_idx + 1 + state->scores[cur_recipe_idx]) % state->score_count;
  }
}

char* extract_candidate(State *state, int idx, int len) {
  char *candidate = malloc(1 + (sizeof(char) * len));
  for (int i = 0; i < len; i++) {
    sprintf(candidate + i, "%d", state->scores[idx + i]);
  }
  return candidate;
}

int search_match(State *state, char *search) {
  int found = -1;

  int len = strlen(search);
  int idx = state->score_count - len;
  int minIdx = idx - (2 * len);
  while (idx >= 0 && idx > minIdx) {
    char *candidate = extract_candidate(state, idx, len);
    /* printf("DEBUG: comparing candidate=%s to search=%s at i=%d\n", candidate, search, idx); */
    if (strcmp(search, candidate) == 0) {
      found = idx;
    }
    free(candidate);
    if (found >= 0) {
      break;
    }
    idx -= 1;
  }

  return found;
}

void print_state(State *state) {
  char *scores = malloc(1 + (sizeof(char) * state->score_count));
  for(int i = 0; i < state->score_count; i++) {
    sprintf(scores + i, "%d", state->scores[i]);
  }
  printf(
    "State: t=%d e=[%d, %d] s=%s\n",
    state->ticks,
    state->elves[0],
    state->elves[1],
    scores
  );
  free(scores);
};

int main (int argc, char *argv[]) {
  if (argc < 2) {
    printf("argument expected\n");
    return 1;
  }

  printf("searching for %s\n", argv[1]);
  State *state = mkstate();
  /* print_state(state); */

  int progress_printed = 0;
  int found_idx = -1;
  while (found_idx < 0) {
    tick(state);
    /* print_state(state); */
    found_idx = search_match(state, argv[1]);

    if (state->ticks % 10000 == 0) {
      if (progress_printed != 0) {
        printf("\033[1A\033[");
      }
      progress_printed = 1;
      printf("_Progress: ran %d ticks, have %d recipes\n", state->ticks, state->score_count);
    }

    // prevent runaway if I missed the answer
    if (state->score_count > (SCORES_BUFFER_SIZE - 3)) {
      printf("Oops, we ran for too long, something's wrong.\n");
      return 1;
    }
  }

  printf("Found the sequence at %d\n", found_idx);

  free(state);
  return 0;
}
