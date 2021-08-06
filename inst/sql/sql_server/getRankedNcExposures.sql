{DEFAULT @n_controls = 50}

with ingredient_concept_evidence as (
    SELECT ims.ingredient_concept_id, max(ims.evidence_exists) as evidence_exists
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

    {@use_siblings} ? {
    UNION

    SELECT
        ms.ingredient_concept_id,
        max(ms.evidence_exists) as evidence_exists
    FROM @cem_schema.matrix_summary ms
    INNER JOIN @vocabulary.concept_ancestor ca1 ON (ca1.descendant_concept_id = ms.condition_concept_id)
    INNER JOIN @vocabulary.concept_ancestor ca2 ON (ca2.ancestor_concept_id = ca1.ancestor_concept_id AND ca2.max_levels_of_separation <= @sibling_lookup_levels)
    WHERE ca2.descendant_concept_id IN (@condition_concept_desc)
    GROUP BY ms.ingredient_concept_id
    }
    ) ims
    GROUP BY ims.ingredient_concept_id
)

-- We need the set of allowed concepts for the entire concept set

SELECT TOP @n_controls
    iq.drug_concept_id as concept_id,
    c.concept_name
FROM (
    -- get concept, and rankings
    {@condition_concept_no_desc != ''} ? {
    SELECT nlcu.drug_concept_id, nlcu.sort_order
    FROM @cem_schema.nc_lu_concept_universe nlcu
    WHERE nlcu.condition_concept_id IN (@condition_concept_no_desc)
    }
    {@condition_concept_no_desc != '' & @condition_concept_desc != ''} ? { UNION }
    {@condition_concept_desc != ''} ? {
    SELECT nlcu.drug_concept_id, nlcu.sort_order
    FROM @cem_schema.nc_lu_concept_universe nlcu
    INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = nlcu.condition_concept_id )
    WHERE ca.ancestor_concept_id  IN (@condition_concept_desc)
    }
    {@use_siblings} ? {
    UNION
    SELECT nlcu.drug_concept_id, nlcu.sort_order
    FROM @cem_schema.nc_lu_concept_universe nlcu
    INNER JOIN @vocabulary.concept_ancestor ca1 ON (ca1.descendant_concept_id = nlcu.condition_concept_id)
    INNER JOIN @vocabulary.concept_ancestor ca2 ON (ca2.ancestor_concept_id = ca1.ancestor_concept_id AND ca2.max_levels_of_separation <= @sibling_lookup_levels)
    WHERE ca2.descendant_concept_id IN (@condition_concept_desc)
    }
) iq
INNER JOIN ingredient_concept_evidence ies ON (iq.drug_concept_id = ies.ingredient_concept_id AND ies.evidence_exists = 0)
INNER JOIN @vocabulary.concept c ON c.concept_id = iq.drug_concept_id
GROUP BY iq.drug_concept_id, c.concept_name
ORDER BY min(iq.sort_order) ASC;
