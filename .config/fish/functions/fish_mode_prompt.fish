function fish_mode_prompt
  switch $fish_bind_mode
    case default
      set_color --bold "#81A1C1"
      echo '[N]'
    case insert
      set_color --bold "#A3BE8C"
      echo '[I]'
    case replace_one
      set_color --bold green
      echo '[R]'
    case visual
      set_color --bold "#B48EAD"
      echo '[V]'
    case '*'
      set_color --bold red
      echo '?'
  end
  set_color normal
end
