/*********
 * made by Matthias Hurrle (@atzedent)
 */

/** @type {HTMLCanvasElement} */
const canvas = window.canvas
const gl = canvas.getContext("webgl2")
const dpr = Math.max(1, .5*window.devicePixelRatio)

const vertexSource = `#version 300 es
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

in vec2 position;

void main(void) {
    gl_Position = vec4(position, 0., 1.);
}
`
const fragmentSource = `#version 300 es
/*********
* made by Matthias Hurrle (@atzedent)
*/
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

out vec4 fragColor;

uniform vec2 resolution;
uniform float time;

#define S smoothstep
#define T mod(time, 200.)
#define syl(p, s) (length(p)-s)

mat3 rotX(float a) {
    float s = sin(a), c = cos(a);

    return mat3(
        vec3(1, 0, 0),
        vec3(0, c,-s),
        vec3(0, s, c)
    );
}

mat3 rotY(float a) {
    float s = sin(a), c = cos(a);

    return mat3(
        vec3(c, 0, s),
        vec3(0, 1, 0),
        vec3(-s,0, c)
    );
}

mat3 rotZ(float a) {
    float s = sin(a), c = cos(a);

    return mat3(
        vec3(c, s, 0),
        vec3(-s,c, 0),
        vec3(0, 0, 1)
    );
}

float rnd(float t) {
	return fract(sin(t*427.771)*232.522);
}

float curve(float t, float d) {
	t /= d;
	return mix(rnd(floor(t)), rnd(floor(t)+1.),
		pow(S(.0, 1.,fract(t)), 10.));
}

float box(vec3 p, vec3 s, float r) {
    p = abs(p) - s;
    return length(max(vec3(0), p))+min(.0, max(max(p.x, p.y), p.z))-r;
}

float mat = .0;
float map(vec3 p) {
    p *= rotZ(2.*acos(-1.)*curve(T*.5, 10.));
    vec3 st = p;
    p.z += T;
    p.z = mod(p.z, 4.4)-2.2;
    vec3 q = p;
    float t = T*2.;
    float sph0 = syl(st-vec3(.125*sin(t), .125*cos(t), -1.*(sin(T)*.125+.8)), .25);
    float sph1 = syl(st-vec3(0), .25);
    float sph2 = syl(st-vec3(.125*cos(t), .125*sin(t), 1.*(cos(T)*.125+.8)), .25);
    float sph = min(min(sph0, sph1), sph2);
    float cyl = syl(q.xy, .5);
    float cylo = max(syl(p.xy, 1.25), abs(p.z)-1.);
    float box = max(-box(p, vec3(.7), .05), cylo);

    float d = min(max(box, -cyl), sph);
    if (d == sph) mat = 1.;

    return d;
}

vec3 norm(vec3 p) {
    vec2 e = vec2(1e-4, 0);
    float d = map(p);
    vec3 n = d - vec3(
        map(p-e.xyy),
        map(p-e.yxy),
        map(p-e.yyx)
    );

    return normalize(n);
}

vec3 dir(vec2 uv, vec3 ro, vec3 target, float zoom) {
    vec3 up = vec3(0,1,0),
    f = normalize(target - ro),
    r = normalize(cross(up, f)),
    u = cross(f, r),
    c = f*zoom,
    i = c+uv.x*r+uv.y*u,
    d = normalize(i);

    return d;
}

void main(void) {
    vec2 uv = (
        gl_FragCoord.xy - .5 * resolution.xy
    ) / min(resolution.x, resolution.y);

    float prog = curve(T*9., 10.);
    float anim = floor(mod(prog-.5, 2.));
    vec3 col = vec3(0),
    ro = vec3(0,(anim==.0?0:3),-3);
    vec3 rd = dir(uv, ro, vec3(0), (anim==.0?1.:.5)),
    p = ro;

    float i = .0, side = 1.;
    for(; i<120.; i++) {
        float d = map(p)*side;

        if(d < 1e-4) {
            vec3 n = norm(p)*side,
            r = normalize(rd),
            l = normalize(vec3(ro-1.));

            if (dot(l, n) < .0) l = -l;

            vec3 h = normalize(l-r);

            float fog = S(.0, 1., i/80.);
            float fres = 1.-pow(max(.0, dot(n, l)), 7.);
            col += mix(vec3(.125, .95, 1), vec3(.125,1,.95), mat) * 1.25*fres * 
                max(.0, dot(n, l))*fog*
                (pow(max(.0, dot(n, h)), 22.) +
                pow(max(.0, dot(n, -r)), 33.));

            side = -side;
            vec3 rdo = refract(r, n, 1.+.45*side);

            if(dot(rdo, rdo) == .0) {
                rdo = reflect(r, n);
            }

            rd = rdo;
            d = .1;
        }

        if(d > 20.) {
            break;
        }
        p += rd*d;
    }

    col += S(.5, .85, i/800.);

    fragColor = vec4(col, 1.);
}
`
let time
let buffer
let program
let resolution
let vertices = []

function resize() {
    const { innerWidth: width, innerHeight: height } = window

    canvas.width = width * dpr
    canvas.height = height * dpr

    gl.viewport(0, 0, width * dpr, height * dpr)
}

function compile(shader, source) {
    gl.shaderSource(shader, source)
    gl.compileShader(shader)

    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error(gl.getShaderInfoLog(shader))
    }
}

function setup() {
    const vs = gl.createShader(gl.VERTEX_SHADER)
    const fs = gl.createShader(gl.FRAGMENT_SHADER)

    program = gl.createProgram()

    compile(vs, vertexSource)
    compile(fs, fragmentSource)

    gl.attachShader(program, vs)
    gl.attachShader(program, fs)
    gl.linkProgram(program)

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error(gl.getProgramInfoLog(program))
    }

    vertices = [-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 1.0]

    buffer = gl.createBuffer()

    gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW)

    const position = gl.getAttribLocation(program, "position")

    gl.enableVertexAttribArray(position)
    gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0)

    time = gl.getUniformLocation(program, "time")
    resolution = gl.getUniformLocation(program, "resolution")
}

function draw(now) {
    gl.clearColor(0, 0, 0, 1)
    gl.clear(gl.COLOR_BUFFER_BIT)

    gl.useProgram(program)
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer)

    gl.uniform1f(time, now * 0.001)
    gl.uniform2f(resolution, canvas.width, canvas.height)
    gl.drawArrays(gl.TRIANGLES, 0, vertices.length * 0.5)
}

function loop(now) {
    draw(now)
    requestAnimationFrame(loop)
}

function init() {
    setup()
    resize()
    loop(0)
}

document.body.onload = init
window.onresize = resize