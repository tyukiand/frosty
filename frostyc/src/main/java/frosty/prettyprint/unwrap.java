package frosty.prettyprint;

import java.lang.annotation.*;

/** Unwrap single-element case class when pretty-printing. */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface unwrap {}