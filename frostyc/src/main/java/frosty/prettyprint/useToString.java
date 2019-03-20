package frosty.prettyprint;

import java.lang.annotation.*;

/** Use <code>toString</code> method when pretty-printing. */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface useToString {}