# p-value hacking
# http://support.sas.com/resources/papers/proceedings14/1723-2014.pdf
# http://www.stat.columbia.edu/~gelman/research/unpublished/forking.pdf
# https://link.springer.com/content/pdf/10.3758/BF03194105.pdf
# A Dirty Dozen: Twelve P-Value Misconceptions by Steven Goodman, in Seminars in Hematology 45 (2008)
# https://lindeloev.github.io/tests-as-linear/

library(tidyverse)

##### Vorlesung 3 #####

url <- "http://www.phonetik.uni-muenchen.de/~jmh/lehre/Rdf"
vdata <- read.table(file.path(url, "vdata.txt"))

ggplot(vdata) + aes(x = 0, y = F1) + geom_boxplot() + 
  xlab("") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 15))

##### Uebung 4 #####

# hp.csv was obtained from:
# https://github.com/kazuki-shin/the-sorting-hat/blob/master/data/harrypotter-characters.csv
hp <- read.csv("./data/hp.csv", sep=";", na.strings="")
hp_names <- hp %>% select(id, name)
hp_meta <- hp %>% select(id, gender, species, bloodStatus, birth, death)
hp_looks <- hp %>% select(id, hairColour, eyeColour) %>% 
  rbind(data.frame(id = 141:145, 
                   hairColour = c("blonde", "reddish", "dark brown", "black", "ash blonde"),
                   eyeColour = c("blue", "green", "brown", "grey", "ice-blue"))) %>% 
  filter(!id %in% 60:67)
hp_wiz <- hp %>% select(id, house, wand, patronus) %>% 
  filter(house %in% c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff")) %>% 
  mutate(house = factor(house, levels = c("Gryffindor", "Ravenclaw", "Slytherin", "Hufflepuff")))
hp_measures <- tibble(id = rep(c(1:8, 14, 30, 35, 37, 38, 46, 47, 50, 51, 139), each = 8), 
                      f0 = c(
                        c(280, 262, 123, 111, 108, 103, 105, 93),
                        c(291, 254, 136, 101, 98, 95, 92, 87),
                        c(337, 323, 266, 252, 248, 244, 240, 231),
                        c(rnorm(6, 90, 4), NA, NA),
                        rnorm(8, 75, 2),
                        c(315, 301, 296, 283, 114, 116, 104, 99),
                        c(rnorm(7, 105, 5), NA),
                        rnorm(8, 105, 5),
                        c(rnorm(5, 89, 3), NA, NA, NA),
                        rnorm(8, 240, 5),
                        rnorm(8, 250, 5),
                        rnorm(8, 325, 5),
                        rnorm(8, 97, 2),
                        c(rnorm(7, 86, 3), NA),
                        c(272, 270, 157, 103, 103, 96, 94, 90),
                        c(rnorm(7, 245, 10), NA),
                        rnorm(8, 285, 4),
                        c(rnorm(6, 338, 9), NA, NA)
                      ),
                      year = rep(c(1991:1997, 2016), times = 18))
write.table(hp_looks, file = "data/hp_looks.txt", row.names = F)
write.table(hp_meta, file = "data/hp_meta.txt", row.names = F)
write.table(hp_measures, file = "data/hp_measures.txt", row.names = F)
write.table(hp_names, file = "data/hp_names.txt", row.names = F)
write.table(hp_wiz, file = "data/hp_wiz.txt", row.names = F)

##### Vorlesung 5 #####

# normalverteilung
ggplot(data.frame(x = c(-10, 10))) + aes(x) + xlab("") + ylab("") +
  stat_function(fun = dnorm, size = 1.2, color = "black") + 
  stat_function(fun = dnorm, args = list(mean = 3, sd = 2), size = 1.2, color = "royalblue3") + 
  stat_function(fun = dnorm, args = list(mean = -2, sd = 1.5), size = 1.2, color = "springgreen4") + 
  theme_light()

normal <- rnorm(200)
skew_right <- c(normal[normal > 0] * 2.5, normal[normal < 0])
skew_left <- c(normal[normal < 0] * 2.5, normal[normal > 0])
bimodal <- c(rnorm(100, -3, .25), rnorm(100, 3, .25))
uniform <- runif(200, min = -4, max = 4)

df <- data.frame(skew_right, skew_left, bimodal, uniform) %>% 
  pivot_longer(cols = c("skew_right", "skew_left", "bimodal", "uniform"), 
               names_to = "distribution", values_to = "samples")

# dists
ggplot(df) + aes(x = samples) + 
  geom_density(size = 1.2) + 
  facet_wrap(~distribution, scales = "free_y") + 
  theme_light() + theme(strip.text = element_text(color = "black"),
                        text = element_text(size = 14))

library(plyr)
library(dplyr)
grid <- seq(min(df$samples), max(df$samples), length = 100)
normaldens <- ddply(df, "distribution", function(d) {
  data.frame( 
    samples = grid,
    density = dnorm(grid, mean(d$samples), sd(d$samples))
  )
})

# dists_norm
ggplot(df) + aes(x = samples) + 
  geom_density(size = 1.2) + 
  geom_line(data = normaldens, aes(y = density), size = 1.2, color = "royalblue3") + 
  facet_wrap(~distribution, scales = "free") + 
  theme_light() + theme(strip.text = element_text(color = "black"),
                        text = element_text(size = 14))

# dists_qq
ggplot(df) + aes(sample = samples) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~distribution, scales = "free_y") +
  ylab("samples") + xlab("theoretical quantiles") +
  theme_light() + theme(strip.text = element_text(color = "black"),
                        text = element_text(size = 14))

# area_norm
ggplot() +
  xlim(-4, 4) + 
  xlab("Standardabweichung") +
  ylab("Wahrscheinlichkeitsdichte") + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                inherit.aes = F) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-1, 1), fill = "cornflowerblue", alpha = 0.5) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-2, -1), fill = "violetred4", alpha = 0.5) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(1, 2), fill = "violetred4", alpha = 0.5) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-3, -2), fill = "goldenrod", alpha = 0.5) + 
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(2, 3), fill = "goldenrod", alpha = 0.5) + 
  annotate(geom = "text", x = 0.5, y = 0.2, label = "0.34") + 
  annotate(geom = "text", x = -0.5, y = 0.2, label = "0.34") + 
  annotate(geom = "text", x = 1.5, y = 0.05, label = "0.135") + 
  annotate(geom = "text", x = -1.5, y = 0.05, label = "0.135") + 
  annotate(geom = "text", x = 2.3, y = 0.01, label = "0.02") + 
  annotate(geom = "text", x = -2.3, y = 0.01, label = "0.02") + 
  geom_vline(xintercept = 0, lty = "dashed") + 
  theme_light()

url <- "http://www.phonetik.uni-muenchen.de/~jmh/lehre/Rdf"
df <- read.table(file.path(url, "vdata.txt")) %>% as_tibble() %>% 
  rename(vokal = V, spannung = Tense, konsonant = Cons, tempo = Rate, subject = Subj) %>% 
  mutate(dauer = log(dur)) %>% 
  select(-c(X, Y))
mu <- mean(df$dauer)
SE <- sd(df$dauer)

# norm_area1
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) + ylab("Wahrscheinlichkeitsdichte") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(3.5, 4.5), fill = "cornflowerblue") +
  theme_light()

# norm_area2
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) + ylab("Wahrscheinlichkeitsdichte") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(5.1, 6), fill = "cornflowerblue") +
  theme_light()

# norm_area3
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) + ylab("Wahrscheinlichkeitsdichte") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(4.9, 5.5), fill = "cornflowerblue") +
  theme_light()

# norm_area4
ggplot(df) +
  aes(x = dauer) + xlim(3.5, 6.5) + ylab("Wahrscheinlichkeitsdichte") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE)) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(3.5, 4.35), fill = "cornflowerblue") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = SE), geom = "area", xlim = c(5.47, 6.5), fill = "cornflowerblue") +
  annotate(geom="text", x=4.9, y=0.5, label="Fläche = 0.95") +
  annotate(geom="text", x=3.9, y=0.1, label="Fläche = 0.025", color = "cornflowerblue") +
  annotate(geom="text", x=5.9, y=0.1, label="Fläche = 0.025", color = "cornflowerblue") +
  theme_light()

##### Vorlesung 6 #####

queen <- read.table(file.path(url, "queen.txt")) %>% as_tibble()
queen.lm <- lm(f0 ~ Alter, data = queen)

alter <- queen %>% filter(Alter > 30 & Alter < 41) %>% pull(Alter)
queen_fitted <- data.frame(Alter = alter,
                           f0 = predict(queen.lm, data.frame(Alter = alter)))
ggplot(queen %>% filter(Alter > 30 & Alter < 41)) +
  aes(x = Alter, y = f0) + 
  geom_point(size = 4) + 
  geom_abline(slope = coef(queen.lm)[2], intercept = coef(queen.lm)[1], color = "blue") + 
  geom_point(data = queen_fitted, color = "red", size = 4) + 
  geom_segment(aes(x = 31, y = 261.5, xend = 31, yend = 254.9), lty = "dashed") + 
  geom_segment(aes(x = 32, y = 259.6, xend = 32, yend = 253.8), lty = "dashed") + 
  geom_segment(aes(x = 33, y = 248.9, xend = 33, yend = 252.8), lty = "dashed") + 
  geom_segment(aes(x = 34, y = 257.0, xend = 34, yend = 251.7), lty = "dashed") + 
  geom_segment(aes(x = 35, y = 243.0, xend = 35, yend = 250.6), lty = "dashed") + 
  geom_segment(aes(x = 37, y = 235.5, xend = 37, yend = 248.5), lty = "dashed") + 
  geom_segment(aes(x = 38, y = 224.8, xend = 38, yend = 247.4), lty = "dashed") + 
  geom_segment(aes(x = 39, y = 237.6, xend = 39, yend = 246.3), lty = "dashed") + 
  geom_segment(aes(x = 40, y = 228.7, xend = 40, yend = 245.2), lty = "dashed") + 
  scale_x_continuous(breaks = seq(30, 40, by = 2)) +
  theme_light()

ggplot(queen %>% filter(Alter > 30 & Alter < 41)) +
  aes(x = Alter, y = f0) + 
  geom_point(size = 4) + 
  geom_abline(slope = coef(queen.lm)[2], intercept = coef(queen.lm)[1], color = "blue") + 
  geom_abline(slope = 0, intercept = mean(queen$f0), color = "orange", size = 1.2) + 
  geom_point(data = queen_fitted, color = "red", size = 4) + 
  geom_segment(aes(x = 31, y = 261.5, xend = 31, yend = 254.9), lty = "dashed") + 
  geom_segment(aes(x = 32, y = 259.6, xend = 32, yend = 253.8), lty = "dashed") + 
  geom_segment(aes(x = 33, y = 248.9, xend = 33, yend = 252.8), lty = "dashed") + 
  geom_segment(aes(x = 34, y = 257.0, xend = 34, yend = 251.7), lty = "dashed") + 
  geom_segment(aes(x = 35, y = 243.0, xend = 35, yend = 250.6), lty = "dashed") + 
  geom_segment(aes(x = 37, y = 235.5, xend = 37, yend = 248.5), lty = "dashed") + 
  geom_segment(aes(x = 38, y = 224.8, xend = 38, yend = 247.4), lty = "dashed") + 
  geom_segment(aes(x = 39, y = 237.6, xend = 39, yend = 246.3), lty = "dashed") + 
  geom_segment(aes(x = 40, y = 228.7, xend = 40, yend = 245.2), lty = "dashed") + 
  scale_x_continuous(breaks = seq(30, 40, by = 2)) + 
  theme_light()

##### Übung 6 #####

lose <- data.frame(wert = ceiling(rnorm(5000, mean = 2000, sd = 500)))
write.table(lose, file = "data/lose.txt", row.names = F)

##### VL 9 #####

library(faux)
library(tidyverse)
library(magrittr)
set.seed(636)
dat <- rnorm_multi(n = 1500, 
                   mu = c(230, 60, 300),
                   sd = c(30, 10, 80),
                   r = c(0.65, -0.31, 0.4),
                   varnames = c("f0", "dB", "dur"),
                   empirical = FALSE)

d1 <- dat %>% filter(dur < 220 & dB > 55) %>% mutate(f0 = f0*rnorm(107, 1.2, 0.15)) 
d2 <- dat %>% filter(dur < 220 & dB > 40 & dB < 55) %>% mutate(f0 = f0*rnorm(107, 1.1, 0.1)) 
d3 <- dat %>% filter(dur > 400 & dB > 60) %>% mutate(f0 = f0*rnorm(119, 0.8, 0.1)) 
d4 <- dat %>% filter(dur > 400 & dB < 60) %>% mutate(f0 = f0*rnorm(36, 0.9, 0.08)) 

df <- rbind(dat, d1, d2, d3, d4)
write.table(df, file = "data/faux.txt", row.names = F)

# Wir müssen aber auch verstehen, was die negative Interaktion für die jeweiligen anderen Steigungen bedeutet. Da die Steigung für `dB` positiv ist, die Interaktion aber negativ, bedeutet das, dass der positive Effekt von Lautstärke auf Grundfrequenz schwächer ist für hohe Dauerwerte. Dies stimmt mit unserem visuellen Eindruck überein: Wenn wir eine Regressionslinie durch die eher hellblauen und eine weitere durch die eher dunkelblauen Punkte in der Abbildung oben legen würden, wäre die Steigung der imaginären hellblauen Regressionslinie niedriger als die der dunkelblauen Regressionslinie. Für die Steigung der Variable `dur` bedeutet die negative Interaktion eine Intensivierung des negativen Effekts von Dauer auf Grundfrequenz für hohe `dB`-Werte.


lm1 <- lm(f0 ~ dB + dur, data = faux)
k <- lm1 %>% tidy() %>% filter(term == "(Intercept)") %>% pull(estimate)
b_dB <- lm1 %>% tidy() %>% filter(term == "dB") %>% pull(estimate)
b_dur <- lm1 %>% tidy() %>% filter(term == "dur") %>% pull(estimate)
high_dur <- 450
low_dur <- 150
intercept_high_dur <- k + b_dB * 0 + b_dur * high_dur
intercept_high_dur
intercept_low_dur <- k + b_dB * 0 + b_dur * low_dur
intercept_low_dur
slope <- b_dB
slope

p1 <- ggplot(faux) + 
  aes(x = dB, y = f0, col = dur) + 
  geom_point() + 
  xlim(0, 95) +
  ylim(0, 500) +
  geom_abline(slope = slope, intercept = intercept_high_dur, color = "#56B1F7", size = 1.2) +
  geom_abline(slope = slope, intercept = intercept_low_dur, color = "#132B43", size = 1.2) +
  geom_vline(xintercept = 0, lty = "dashed") +
  theme_light()

lm2 <- lm(f0 ~ dB * dur, data = faux)
k <- lm2 %>% tidy() %>% filter(term == "(Intercept)") %>% pull(estimate)
b_dB <- lm2 %>% tidy() %>% filter(term == "dB") %>% pull(estimate)
b_dur <- lm2 %>% tidy() %>% filter(term == "dur") %>% pull(estimate)
b_interaction <- lm2 %>% tidy() %>% filter(term == "dB:dur") %>% pull(estimate)
intercept_low_dur <- k + b_dB * 0 + b_dur * low_dur + b_interaction * (0 * low_dur)
intercept_low_dur
intercept_high_dur <- k + b_dB * 0 + b_dur * high_dur + b_interaction * (0 * high_dur)
intercept_high_dur
slope_low_dur <- b_dB + b_interaction * low_dur
slope_low_dur
slope_high_dur <- b_dB + b_interaction * high_dur
slope_high_dur

p2 <- ggplot(faux) + 
  aes(x = dB, y = f0, col = dur) + 
  geom_point() + 
  xlim(0, 95) +
  ylim(0, 500) +
  geom_abline(slope = slope_low_dur, intercept = intercept_low_dur, color = "#132B43", size = 1.2) +
  geom_abline(slope = slope_high_dur, intercept = intercept_high_dur, color = "#56B1F7", size = 1.2) +
  geom_vline(xintercept = 0, lty = "dashed") + 
  theme_light()

library(gridExtra)
grid.arrange(p1, p2, nrow = 1)

##### VL 11 #####

library(tidyverse)
url <- "http://www.phonetik.uni-muenchen.de/~jmh/lehre/Rdf"
int <- read.table(file.path(url, "dbwort.df.txt"), stringsAsFactors = T) %>% 
  as_tibble() %>% 
  rename(vowel = V, gender = G, subject = Vpn, word = Wort)

x1 <- int %>% filter(word == "w1" & vowel == "a")
x2 <- int %>% filter(word == "w2" & vowel == "a")
x3 <- int %>% filter(word == "w3" & vowel == "a")
x4 <- int %>% filter(word == "w4" & vowel == "i")
x5 <- int %>% filter(word == "w5" & vowel == "i")
x6 <- int %>% filter(word == "w6" & vowel == "i")
x1.1 <- int %>% filter(word == "w1" & vowel == "i") %>% mutate(word = "w4")
x2.1 <- int %>% filter(word == "w2" & vowel == "i") %>% mutate(word = "w5")
x3.1 <- int %>% filter(word == "w3" & vowel == "i") %>% mutate(word = "w6")
x4.1 <- int %>% filter(word == "w4" & vowel == "a") %>% mutate(word = "w1")
x5.1 <- int %>% filter(word == "w5" & vowel == "a") %>% mutate(word = "w2")
x6.1 <- int %>% filter(word == "w6" & vowel == "a") %>% mutate(word = "w3")

x <- rbind(x1, x2, x3, x4, x5, x6,
           x1.1, x2.1, x3.1, x4.1, x5.1, x6.1)

write.table(x, file = "data/int_new.txt", row.names = F)

##### UE 11 #####

set.seed(643)
phr <- read.table(file.path(url, "phr.df.txt"), stringsAsFactors = T)
p <- rbind(phr %>% filter(tempo < 4.3) %>% mutate(tempo = tempo * rnorm(423, 1, 0.15)),
           phr %>% filter(tempo > 4.3) %>% mutate(tempo = tempo * rnorm(477, 1.2, 0.15)))
p %<>% mutate(tempo = ifelse(tempo < 0, tempo * -1, tempo),
              age = ifelse(alter %in% c(23, 25, 28, 29, 32, 33, 36, 38), "young", "old")) %>% 
  rename(subject = Vpn, word = Ag, gender = G, rate = tempo) %>%
  select(subject, word, rate, gender, age) %>%
  as_tibble()
p1 <- rbind(p %>% filter(age == "old" & gender == "F") %>% mutate(rate = rate * rnorm(300, 0.8, 0.1)), 
            p %>% filter(age == "old" & gender == "M") %>% mutate(rate = rate * rnorm(210, 1.1, 0.1)),
            p %>% filter(age == "young"))
ggplot(p1) + aes(x = age, y = rate, fill = gender) + geom_boxplot()

write.table(p1, file = "data/speechRate.txt", row.names = F)
# ggplot(df %>% filter(vowel == "a")) + 
#   aes(x = gender, y = db) + 
#   geom_point() + 
#   facet_wrap(~word) + 
#   geom_point(data = df %>% 
#                filter(vowel == "a") %>% 
#                group_by(word, gender) %>% 
#                summarise(db = mean(db)),
#              color = "orange", size = 3, stroke = 2, shape = 4) + 
#   geom_line(data = df %>% 
#               filter(vowel == "a") %>% 
#               group_by(word, gender) %>% 
#               summarise(db = mean(db)),
#             aes(x = gender, y = db, group = word),
#              lty = "dashed")


##### Lösung 11 #####

# Wortspezifische Random Slope für die Interaktion zwischen `age` und `gender`? (age * gender | word)
ggplot(df) + 
  aes(x = age, y = rate) + 
  geom_point() + 
  facet_wrap(~word, nrow = 2) + 
  geom_point(data = df %>% 
               group_by(word, age, gender) %>% 
               summarise(rate = mean(rate)),
             mapping = aes(col = gender),
             size = 3, stroke = 2, shape = 4) + 
  geom_line(data = df %>% 
              group_by(word, age, gender) %>% 
              summarise(rate = mean(rate)),
            mapping = aes(col = gender, group = interaction(word, gender)),
            lty = "dashed")

ggplot(df) + 
  aes(x = gender, y = rate) + 
  geom_point() + 
  facet_wrap(~word, nrow = 2) + 
  geom_point(data = df %>% 
               group_by(word, age, gender) %>% 
               summarise(rate = mean(rate)),
             mapping = aes(col = age),
             size = 3, stroke = 2, shape = 4) + 
  geom_line(data = df %>% 
              group_by(word, age, gender) %>% 
              summarise(rate = mean(rate)),
            mapping = aes(col = age, group = interaction(word, age)),
            lty = "dashed")




# ## Schätzungen ausgeben lassen
# 
# Wir können uns die Fixed Effects auch mit der Funktion `fixef()` ausgeben lassen, die auf das Ergebnis von `lmer()` angewendet wird:
#   
#   ```{r}
# df.lmer %>% fixef()
# ```
# 
# Interessanter ist die Funktion `ranef()`, die die Random Effects ausgibt -- allerdings nicht in der zusammengefassten Form, wie es `summary()` tut:
#   
#   ```{r}
# random_effects <- df.lmer %>% ranef()
# random_effects
# ```
# 
# Das Ergebnis von `ranef()` besteht aus so vielen Tabellen wie es Random Effects im Modell gibt; hier also eine Tabelle für `subject` und eine für `word`. Fangen wir mit letzterer an:
#   
#   ```{r}
# ran_ef_word <- random_effects$word
# ran_ef_word
# ```
# 
# Dies ist ein Data Frame mit einer Spalte und sechs Zeilen. Die Zeilennamen sind die "Namen" der sechs Wörter:
#   
#   ```{r}
# ran_ef_word %>% rownames()
# ```
# 
# Die Werte in der Spalte `(Intercept)` sind die *Änderungen*, die dem allgemeinen Intercept zugefügt werden müssen, um wortspezifische I
# 
# Fangen wir mit der Tabelle für `subject` an.
# 
# ```{r}
# ran_ef_subject <- random_effects$subject
# ran_ef_subject
# ```
# 
# Dieser Data Frame besteht aus zwei Spalten, eine für das Random Intercept und eine für die Random Slope, weil wir ja den Random Effect `(1 + vowel | subject)` für das Modell gewählt hatten. Die Zeilennamen sind die Namen der zehn Versuchspersonen:
#   
#   ```{r}
# ran_ef_subject %>% rownames()
# ```
# 
# Die beiden Spalten der Tabelle `ran_ef_subject` enthalten die *Änderungen* für das Intercept und die Vokal-Slope 
# 
# ```{r}
# df %>% 
#   filter(vowel == "a") %>% 
#   group_by(subject) %>% 
#   summarise(m = mean(db))
# ```
# 
# 
# <div class="green">
#   **Weiterführende Infos: Spaltennamen mit Sonderzeichen**
#   
#   Weil der Spaltennamen für das Random Intercept leider Sonderzeichen enthält (die runden Klammern), müssen wir beim Auswählen der Spalte durch das Dollarzeichen den Spaltennamen in die einfachen Backticks setzen:
#   
#   ```{r}
# ran_ef_subject$`(Intercept)`
# ```
# </div>
#   
#   