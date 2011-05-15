(in-package :shanai.po.bot.user-warn-patterns)

(defparameter *whitelistusernames*
  (list "BooBerry" "Grahamthesexykid" "Jass" "Pablocucumber" "oldfart"
        "Trizzyscummbaggs" "Sex_Adon" "Trollface" "Manaleo808" "Pokeballs4Food"
        "SexyBabyPanda" "-Psychoanalyst-" "sexysceptilez" "SirPsychoSexy"
        "Betch Slutemberg" "xXSexyHeartsXx" "foo" "thatsexywalnut" "zeroality[fucker]"
        "balls" "sexygirlzz" "tra tra trollin" "LetsTestThisShit" "homosexual"
        "Andisex" "mindblown" "tic tok trololo" "physicstroll" "!psychotherapist!"
        "!psycho!therapist!" "hideyoshi kinoshita" "Sexy Haunter" "slutty pumpkin"
        "hancocko" "Trololololo" "sex-ray vision" "Sexy Baby Panda" "Popsicle [Troll]"
        "GiantTroll" "Metrosexual Hipster" "Wallytrolly" "ninjatroll" "poppycock"
        "mister trololo" "efosex" "bLow808" "WxWxWx" "coyotte508" "janus"
        "Trollololin'" "noobsaibot" "the troller" "fakehaydentroll" "HITCHCOCK"
        "drew peacock" "arcanus" "i sexy i - tst" "sexy legs" "jj redick" "alsex"
        "sexy cheff" "TheRobot"))

(defun whitelisted-username-list ()
  *whitelistusernames*)
(defparameter *warnpatterns*
  (list "(fuck)"
        "(THOMAS)" ; annoying troll
        "(badass)"
        "^(Fuk)"
        "(sh[i1]t)"
        "(anus)"
        "(anal)"
        "(rapist)"
        "(blow)"
        "(Bot\\d+)$"
        "(nigga)"
        "(n[i1]gg[e3]r)" 
        "(cunt)" 
        "(wh[o0]re)"
        "(d[i1]ck)"
        "(puta)"
        "(p[e3]n[i1]s)"
        "(Balls)"
        "(fag)" 
        "(\\[[^\\]]+\\]\\s*Tr[o0][l1i][o0][l1i])"
        "(\\bass+\\b)" 
        "(tits)"
        "(fart)"
        "(deepthro[au]t)"
        "(staff)"
        "(d[i1][l1]d[o0])"
        "(b[0o][0o]+b)" 
        "(cl[i1]t)" 
        "(c[o0]ck)" 
        "(cum)"
        "(s[l1]ut)"
        "(qu[3e][3e]+f)"
        "(d[o0]uche)"
        "(b[i1]tch)"
        "(n[i1]gg[3e]r)"
        "(s[e3]x)"
        "(卐)"))

(defun blacklisted-username-pattern-list ()
  *warnpatterns*)

(defparameter *case-sensitive-warn-patterns*
  (list))


(defun whitelist-username (name)
  "Add NAME to the username whitelist.

Returns NIL in the case that the username is already in the whitelist."
  (declare (type string name))
  (unless (find name (whitelisted-username-list) :test #'string-equal)
    (alexandria:appendf *whitelistusernames* (list name))))

