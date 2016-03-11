# Git prompt is super duper slow
#PROMPT='%{$fg[cyan]%}%n@%m%{$reset_color%}:%{$fg[blue]%}%~$(git_prompt_info)%{$reset_color%}$ '

function my_git_prompt() {
  git symbolic-ref HEAD >& /dev/null
  if [ $? -eq 0 ]; then
    echo " ($(git_current_branch))"
  fi
}

PROMPT='%{$fg[cyan]%}%n@%m%{$reset_color%}:%{$fg[blue]%}%~%{$fg[yellow]%}$(my_git_prompt)%{$reset_color%}$ '

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"

