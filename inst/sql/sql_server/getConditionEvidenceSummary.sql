SELECT ims.ingredient_concept_id, c.concept_name, max(ims.evidence_exists) as evidence_exists
FROM (
    -- Where children of the ingredient concept are explicitly included
    SELECT
        ms.ingredient_concept_id,
        max(ms.evidence_exists) as evidence_exists
    FROM @cem_schema.matrix_summary ms
    INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = ms.condition_concept_id)
    WHERE ca.ancestor_concept_id IN (@condition_concept_desc)
    GROUP BY ms.ingredient_concept_id
    -- Where children of the ingredient concept are explicitly excluded
    {@condition_concept_no_desc != ''} ? {
    UNION

    SELECT
        ms.ingredient_concept_id
        max(ms.evidence_exists) as evidence_exists
    FROM @cem_schema.matrix_summary ms
    WHERE ms.condition_concept_id IN (@condition_concept_no_desc)
    GROUP BY ms.ingredient_concept_id
    }

    {@use_siblings > 0} ? {
    UNION

    SELECT
        ms.ingredient_concept_id,
        max(ms.evidence_exists) as evidence_exists
    FROM @cem_schema.matrix_summary ms
    INNER JOIN @vocabulary.concept_ancestor ca1 ON (ca1.descendant_concept_id = ms.condition_concept_id)
    INNER JOIN @vocabulary.concept_ancestor ca2 ON (ca2.ancestor_concept_id = ca1.ancestor_concept_id AND ca2.max_levels_of_separation <= @use_siblings)
    WHERE ca2.descendant_concept_id IN (@condition_concept_desc)
    GROUP BY ms.ingredient_concept_id
    }
) ims
INNER JOIN @vocabulary.concept c ON (c.concept_id = ims.ingredient_concept_id)
GROUP BY ims.ingredient_concept_id, c.concept_name