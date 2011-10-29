package org.onion_lang.jasson

/*
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/10/29
 * Time: 9:53
 */
trait CodeGenerator[Schema, Settings, Output] extends ((Schema, Settings) => Output)
