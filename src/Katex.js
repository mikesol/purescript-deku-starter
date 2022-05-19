import katex from "katex";

export const render = (s) => (e) => () =>
	katex.render(s, e, {
		throwOnError: false,
	});