{DEFAULT @n_controls = 50}

-- We need the set of allowed concepts for the entire concept set

SELECT TOP @n_controls
    iq.condition_concept_id as concept_id,
    c.concept_name
FROM (
    -- get concept, and rankings
    {@concept_no_desc != ''} ? {
    SELECT nlcu.condition_concept_id, nlcu.sort_order
    FROM @cem_schema.nc_lu_concept_universe nlcu
    WHERE nlcu.drug_concept_id IN (@concept_no_desc)
    }
    {@concept_no_desc != '' & @concept_desc != ''} ? { UNION }
    {@concept_desc != ''} ? {
    SELECT nlcu.condition_concept_id, nlcu.sort_order
    FROM @cem_schema.nc_lu_concept_universe nlcu
    INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = nlcu.drug_concept_id )

    WHERE ca.ancestor_concept_id  IN (@concept_desc)
    }
) iq
INNER JOIN (
    SELECT ims.condition_concept_id, max(ims.evidence_exists) as evidence_exists
    FROM (
        -- Where children of the ingredient concept are explicitly included
        {@concept_desc != ''} ? {
        SELECT
            ms.condition_concept_id, max(ms.evidence_exists) as evidence_exists
        FROM @cem_schema.matrix_summary ms
        INNER JOIN @vocabulary.concept_ancestor ca ON (ca.descendant_concept_id = ms.ingredient_concept_id)
        WHERE ca.ancestor_concept_id IN (@concept_desc)
        GROUP BY ms.condition_concept_id
        }
        {@concept_no_desc != '' & @concept_desc != ''} ? { UNION }
        {@concept_no_desc != ''} ? {
        -- Where children of the ingredient concept are explicitly excluded
        SELECT
            ms.condition_concept_id, max(ms.evidence_exists) as evidence_exists
        FROM @cem_schema.matrix_summary ms
        WHERE ms.condition_concept_id IN (@concept_no_desc)
        GROUP BY ms.condition_concept_id
        }
    ) ims
    GROUP BY ims.condition_concept_id
) ies ON (iq.condition_concept_id = ies.condition_concept_id AND ies.evidence_exists = 0)
INNER JOIN @vocabulary.concept c ON c.concept_id = iq.condition_concept_id
GROUP BY iq.condition_concept_id, c.concept_name
ORDER BY min(iq.sort_order) ASC;
