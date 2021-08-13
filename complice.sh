#!/usr/bin/sh

# Thanks to this awesome redditor for the picom grayscale scripts:
# https://www.reddit.com/r/archlinux/comments/j2tt7o/how_to_turn_display_grayscale/g79z3ii?utm_source=share&utm_medium=web2x&context=3

TOKEN=$(echo $COMPLICE_TOKEN | base64 -d)
URL="https://complice.co/api/v0/u/me/today/timer/all?auth_token=$TOKEN"
TIMER_STATE=$(curl $URL | jq '.ticker.state')

killall picom

if [ $TIMER_STATE = '"inactive"' ]; then
    sleep 0.1
        picom -b --backend glx --glx-fshader-win "
            uniform sampler2D tex;
                void main() {
                    vec4 c = texture2D(tex, gl_TexCoord[0].xy);
                    float y = dot(c.rgb, vec3(0.299, 0.587, 0.114));
                    gl_FragColor = vec4(y, y, y, 1.0);
               }"
fi
