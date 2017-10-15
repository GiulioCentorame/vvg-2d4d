# look at game variables as function of condition

# deaths
# kills
# distance through maps
# chaingun bullets fired
# shotgun blasts fired
# times wounded

dat %>% 
  filter(!is.na(Difficulty)) %>% 
  gather(key, value, Game.1:Game.6) %>% 
  ggplot(aes(x = Difficulty, y = value)) +
  geom_boxplot() +
  facet_wrap(~key, scales = "free_y")

ggplot(dat, aes(x = Game.6, y = Game.1)) +
  geom_point() +
  geom_smooth()

ggplot(dat, aes(x = Game.6, y = Game.3)) +
  geom_point() +
  geom_smooth()
  
ggplot(dat, aes(x = Game.2, y = Game.3, col = Difficulty)) +
  geom_point() + 
  geom_smooth()
# uh oh. looks like some easy-condition players ran through the map killing very little
# also location code > 22 appears to be a mistake

ggplot(dat, aes(x = Game.2, y = Assignment, col = Violence)) +
  geom_point() +
  geom_smooth()
