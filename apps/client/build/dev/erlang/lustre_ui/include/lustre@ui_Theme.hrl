-record(theme, {
    space :: lustre@ui:size(),
    text :: lustre@ui:size(),
    radius :: lustre@ui:value(),
    primary :: lustre@ui@util@colour:scale(),
    greyscale :: lustre@ui@util@colour:scale(),
    error :: lustre@ui@util@colour:scale(),
    warning :: lustre@ui@util@colour:scale(),
    success :: lustre@ui@util@colour:scale(),
    info :: lustre@ui@util@colour:scale()
}).
