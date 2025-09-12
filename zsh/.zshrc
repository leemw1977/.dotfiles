# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  zsh-completions
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# get machine's ip address
alias ip="ipconfig getifaddr en0"

# edit global zsh configuration
alias zshconfig="vim ~/.zshrc"
# reload zsh configuration
alias zshsource="source ~/.zshrc"
# reload zsh configuration
alias ohmyzsh="cd ~/.oh-my-zsh"

# navigate to global ssh directory
alias sshhome="cd ~/.ssh"
# edit global ssh configuration
alias sshconfig="vim ~/.ssh/config"

# edit global git configuration
alias gitconfig="vim ~/.gitconfig"

# git aliases
alias gits="git status"
alias gitd="git diff"
alias gitl="git lg"
alias gita="git add ."
alias gitc="cz commit"

alias loc="npx sloc --format cli-table --format-option head --exclude 'build|\.svg$\.xml' ./"

# load zsh-completions
autoload -U compinit && compinit

# Add .NET Core SDK tools
if [[ ":$PATH:" != *":$HOME/.dotnet/tools:"* ]]; then
  export PATH="$HOME/.dotnet/tools:$PATH"
fi

# use nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# use starship theme (needs to be at the end)
eval "$(starship init zsh)"

# Created by `pipx`
case "$OSTYPE" in
  darwin*) # macOS
    export PATH="$PATH:/Users/leewilliams/.local/bin"
    ;;
  linux-gnu*) # Linux
    export PATH="$PATH:/home/leewilliams/.local/bin"
    ;;
  msys*|cygwin*|win32) # Windows / Git Bash
    export PATH="$PATH:/c:/Users/leewilliams/.local/bin"
    ;;
  *)
    echo "Warning: Unknown OS type '$OSTYPE' – pipx path not set"
    ;;
esac


# Setup SSH auth sock
export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
