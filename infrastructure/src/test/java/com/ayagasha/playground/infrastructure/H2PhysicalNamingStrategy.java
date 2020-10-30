package com.ayagasha.playground.infrastructure;

import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.boot.model.naming.PhysicalNamingStrategy;
import org.hibernate.cfg.ImprovedNamingStrategy;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;

public class H2PhysicalNamingStrategy extends ImprovedNamingStrategy implements PhysicalNamingStrategy {
    /**
     * Singleton access
     */
    public static final H2PhysicalNamingStrategy INSTANCE = new H2PhysicalNamingStrategy();

    public H2PhysicalNamingStrategy() {

    }

    @Override
    public Identifier toPhysicalCatalogName(Identifier name, JdbcEnvironment context) {
        if (name == null)
            return null;

        return Identifier.toIdentifier("");
    }

    @Override
    public Identifier toPhysicalSchemaName(Identifier name, JdbcEnvironment context) {
        if (name == null)
            return null;

        return Identifier.toIdentifier(name.getText().toUpperCase());
    }

    @Override
    public Identifier toPhysicalTableName(Identifier name, JdbcEnvironment context) {
        if (name == null)
            return null;
        return Identifier.toIdentifier(name.getText().toUpperCase());
    }

    @Override
    public Identifier toPhysicalSequenceName(Identifier name, JdbcEnvironment context) {
        return toUpperCase(name);
    }

    @Override
    public Identifier toPhysicalColumnName(Identifier name, JdbcEnvironment context) {
        return Identifier.toIdentifier(super.columnName(name.getText()).toUpperCase());
    }

    private Identifier toUpperCase(Identifier name) {
        if (name == null)
            return null;
        if (name.isQuoted())
            return name;
        String text = name.getText().trim().toUpperCase();
        return Identifier.toIdentifier(text);
    }
}
