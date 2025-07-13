from manim import *

class DerivativeVisual(Scene):
    def construct(self):
        # Setup
        self.camera.background_color = "#1a1a1a"
        title = Title(
            r"Derivative of \( f(x) = \sin(\sqrt{x}) \) at \( x = \pi^2 \)",
            font_size=40,
            color=WHITE
        )
        self.play(Write(title, run_time=2))
        self.wait(1)

        # Graph setup
        axes = Axes(
            x_range=[0, 15, 3],
            y_range=[-1.5, 1.5, 1],
            axis_config={"color": LIGHT_GRAY},
            x_axis_config={"numbers_to_include": [0, 3, 6, 9, 12, 15]},
            y_axis_config={"numbers_to_include": [-1, 0, 1]},
        ).shift(DOWN)
        axes_labels = axes.get_axis_labels(x_label="x", y_label="f(x)")
        self.play(Create(axes), Write(axes_labels), run_time=2)
        self.wait(1)

        # Plot the function
        func = axes.plot(
            lambda x: np.sin(np.sqrt(x)),
            color=BLUE,
            stroke_width=4
        )
        func_label = MathTex(r"f(x) = \sin(\sqrt{x})", color=BLUE).next_to(func, UR, buff=0.2)
        self.play(Create(func), Write(func_label), run_time=3)
        self.wait(1)

        # Highlight x = π²
        x_val = PI**2
        dot = Dot(axes.c2p(x_val, np.sin(np.sqrt(x_val))), color=YELLOW)
        x_label = MathTex(r"x = \pi^2", color=YELLOW).next_to(dot, UR, buff=0.1)
        self.play(Create(dot), Write(x_label), run_time=2)
        self.wait(1)

        # Break into composite functions (chain rule)
        composite_text = Text("Chain Rule Breakdown", font_size=28, color=GREEN).next_to(title, DOWN)
        self.play(Write(composite_text))
        self.wait(1)

        # Inner and outer functions
        inner_eq = MathTex(r"u = \sqrt{x}", color=MAROON).shift(LEFT*3 + UP)
        outer_eq = MathTex(r"f(u) = \sin(u)", color=PURPLE).shift(RIGHT*3 + UP)
        self.play(Write(inner_eq), Write(outer_eq), run_time=2)
        self.wait(1)

        # Derivatives
        inner_deriv = MathTex(r"\frac{du}{dx} = \frac{1}{2\sqrt{x}}", color=MAROON).next_to(inner_eq, DOWN)
        outer_deriv = MathTex(r"\frac{df}{du} = \cos(u)", color=PURPLE).next_to(outer_eq, DOWN)
        self.play(Write(inner_deriv), Write(outer_deriv), run_time=2)
        self.wait(1)

        # Combine with chain rule
        chain_rule = MathTex(
            r"\frac{df}{dx} = \frac{df}{du} \cdot \frac{du}{dx}",
            color=YELLOW
        ).next_to(composite_text, DOWN)
        self.play(Write(chain_rule), run_time=2)
        self.wait(1)

        # Substitute derivatives
        final_deriv = MathTex(
            r"\frac{df}{dx} = \cos(\sqrt{x}) \cdot \frac{1}{2\sqrt{x}}",
            color=YELLOW
        ).next_to(chain_rule, DOWN)
        self.play(Transform(chain_rule.copy(), final_deriv), run_time=2)
        self.wait(2)

        # Evaluate at x = π²
        evaluate_text = Text("Plugging in x = π²...", font_size=24, color=GRAY).next_to(final_deriv, DOWN)
        self.play(Write(evaluate_text), run_time=1)
        self.wait(1)

        substitution = MathTex(
            r"\frac{df}{dx}\Big|_{x=\pi^2} = \cos(\pi) \cdot \frac{1}{2\pi}",
            color=YELLOW
        ).next_to(evaluate_text, DOWN)
        self.play(Write(substitution), run_time=2)
        self.wait(1)

        simplified = MathTex(
            r"= (-1) \cdot \frac{1}{2\pi} = -\frac{1}{2\pi}",
            color=RED
        ).next_to(substitution, DOWN)
        self.play(Write(simplified), run_time=2)
        self.wait(2)

        # Final result box
        result_box = SurroundingRectangle(simplified, color=BLUE, buff=0.2)
        self.play(Create(result_box), run_time=2)
        self.wait(3)

        # Fade out
        self.play(*[FadeOut(mob) for mob in self.mobjects])