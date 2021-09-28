SELECT
ims.condition_concept_id, c.concept_name, max(ims.evidence_exists) as evidence_exists
FROM (
    -- Where children of the ingredient concept are explicitly included
    {@condition_concept_desc != ''} ? {
    SELECT
        ms.condition_concept_id, max(ms.evidence_exists) as evidence_exists
    FROM @cem_schema.matrix_summary ms
    INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = ms.ingredient_concept_id)
    WHERE ca.ancestor_concept_id IN (@concept_desc)
    GROUP BY ms.condition_concept_id
    }
    {@concept_no_desc != ''} ? {
    {@concept_no_desc != '' & @condition_concept_desc != ''} ? { UNION }
    -- Where children of the ingredient concept are explicitly excluded
    SELECT
        ms.condition_concept_id, max(ms.evidence_exists) as evidence_exists
    FROM @cem_schema.matrix_summary ms
    WHERE ms.condition_concept_id IN (@concept_no_desc)
    GROUP BY ms.condition_concept_id
    }
) ims
INNER JOIN @vocabulary.concept c ON (c.concept_id = ims.condition_concept_id)
GROUP BY ims.condition_concept_id, c.concept_name;
