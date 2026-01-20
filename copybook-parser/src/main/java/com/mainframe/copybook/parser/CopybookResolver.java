package com.mainframe.copybook.parser;

import java.io.Reader;
import java.util.Optional;

/**
 * Abstraction for resolving copybook names to their source content.
 * Implementations may resolve copybooks from the file system, classpath,
 * or any other source.
 */
public interface CopybookResolver {

    /**
     * Resolve a copybook by name and return a Reader for its content.
     *
     * @param copybookName the name of the copybook to resolve
     * @return an Optional containing a Reader if found, empty otherwise
     */
    Optional<Reader> resolve(String copybookName);

    /**
     * A resolver that never finds any copybooks.
     */
    CopybookResolver NONE = name -> Optional.empty();
}
