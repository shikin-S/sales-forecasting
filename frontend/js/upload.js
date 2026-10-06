(function initializeUpload() {
  const maximumFileSize = 10 * 1024 * 1024;
  const apiUrl = "http://127.0.0.1:8000/upload";

  document.addEventListener("DOMContentLoaded", function onUploadLoaded() {
    const input = document.getElementById("dataset-file");
    const dropZone = document.getElementById("drop-zone");
    const uploadButton = document.getElementById("upload-button");
    const selectedFileLabel = document.getElementById("selected-file");
    const status = document.getElementById("upload-status");
    const result = document.getElementById("upload-result");
    const resultOriginalRows = document.getElementById("result-original-rows");
    const resultFinalRows = document.getElementById("result-final-rows");
    const resultMissing = document.getElementById("result-missing");
    const resultDuplicates = document.getElementById("result-duplicates");
    const resultEmptyRows = document.getElementById("result-empty-rows");
    const resultInvalid = document.getElementById("result-invalid");
    const resultCalculated = document.getElementById("result-calculated");
    const resultFile = document.getElementById("result-file");
    const detectedFields = document.getElementById("detected-fields");
    const processingWarning = document.getElementById("processing-warning");
    const profileType = document.getElementById("profile-type");
    const profileShape = document.getElementById("profile-shape");
    const profileDateRange = document.getElementById("profile-date-range");
    const profileForecasting = document.getElementById("profile-forecasting");
    const profileSalesAnalysis = document.getElementById("profile-sales-analysis");
    const profileDuplicateIds = document.getElementById("profile-duplicate-ids");
    const profileDimensions = document.getElementById("profile-dimensions");
    const profileMissingFields = document.getElementById("profile-missing-fields");
    const profileDerivedFields = document.getElementById("profile-derived-fields");
    const profileColumnDetails = document.getElementById("profile-column-details");
    const processingSteps = document.querySelectorAll("[data-step]");
    const fieldLabels = {
      date: "Date",
      total_sales: "Sales / Revenue",
      quantity: "Quantity",
      unit_price: "Unit price",
      product: "Product",
      category: "Category",
      customer: "Customer",
      region: "Region",
      store: "Store",
      transaction_id: "Transaction ID",
      profit: "Profit",
      discount: "Discount",
      cost: "Cost"
    };

    function renderList(element, values, formatter) {
      element.replaceChildren();
      const entries = Array.isArray(values) ? values : typeof values === "string" ? [values] : [];
      entries.forEach(function addValue(value) {
        const item = document.createElement("li");
        item.textContent = formatter ? formatter(value) : String(value);
        element.appendChild(item);
      });
      if (!element.childElementCount) {
        const item = document.createElement("li");
        item.textContent = "None detected";
        element.appendChild(item);
      }
    }

    function setProcessingStep(stepName) {
      processingSteps.forEach(function updateStep(step) {
        step.classList.toggle("is-current", step.dataset.step === stepName);
        step.classList.toggle("is-complete", ["uploading", "validating", "cleaning", "generating"].indexOf(step.dataset.step) < ["uploading", "validating", "cleaning", "generating", "complete"].indexOf(stepName));
      });
    }

    function validateFile(file) {
      if (!file) {
        selectedFileLabel.textContent = "No file selected";
        status.textContent = "Please select a CSV file.";
        return false;
      }
      selectedFileLabel.textContent = file.name;
      if (!file.name.toLowerCase().endsWith(".csv")) {
        status.textContent = "Please upload a CSV file.";
        return false;
      }
      if (file.size > maximumFileSize) {
        status.textContent = "File size exceeds the 10 MB limit.";
        return false;
      }
      status.textContent = "File selected successfully.";
      return true;
    }

    function parseJsonResponse(response, rawText) {
      if (!rawText) {
        return {};
      }

      try {
        return JSON.parse(rawText);
      } catch (error) {
        console.error("Upload response was not valid JSON.", {
          url: apiUrl,
          status: response.status,
          contentType: response.headers.get("content-type"),
          body: rawText
        });
        return { message: rawText || "The server returned an invalid response." };
      }
    }

    function ensureHttpOrigin() {
      if (window.location.protocol === "file:") {
        status.textContent = "Open this app through a local web server before uploading. Browsers block fetch() calls from file:// pages to http://127.0.0.1:8000.";
        console.error("Upload blocked because the page was opened from a file:// URL.", {
          url: window.location.href,
          apiUrl: apiUrl
        });
        return false;
      }
      return true;
    }

    input.addEventListener("change", function handleFileSelection(event) {
      validateFile(event.target.files[0]);
    });
    uploadButton.addEventListener("click", async function uploadSelectedFile() {
      if (!ensureHttpOrigin()) {
        return;
      }

      const selectedFile = input.files[0];
      if (!validateFile(selectedFile)) return;

      uploadButton.disabled = true;
      status.textContent = "Uploading and processing dataset...";
      setProcessingStep("uploading");
      result.hidden = true;

      const formData = new FormData();
      formData.append("file", selectedFile);

      try {
        setProcessingStep("validating");
        console.info("Submitting upload request", {
          url: apiUrl,
          fileName: selectedFile.name,
          fileSize: selectedFile.size
        });

        const response = await fetch("http://127.0.0.1:8000/upload", {
          method: "POST",
          body: formData
        });

        const responseContentType = response.headers.get("content-type") || "";
        const responseText = await response.text();
        const responseBody = parseJsonResponse(response, responseText);

        console.info("Upload response received", {
          url: apiUrl,
          status: response.status,
          ok: response.ok,
          contentType: responseContentType,
          body: responseText || responseBody
        });

        if (!response.ok) {
          const message = responseBody.message || responseText || "An error occurred while processing the dataset.";
          status.textContent = message;
          console.error("Dataset upload failed:", {
            url: apiUrl,
            status: response.status,
            contentType: responseContentType,
            responseBody: responseBody,
            rawBody: responseText
          });
          return;
        }

        if (!responseBody.success) {
          const message = responseBody.message || "An error occurred while processing the dataset.";
          status.textContent = message;
          console.error("Dataset upload error payload:", {
            url: apiUrl,
            status: response.status,
            contentType: responseContentType,
            responseBody: responseBody,
            rawBody: responseText
          });
          return;
        }

        setProcessingStep("generating");
        status.textContent = responseBody.message || "Dataset processed successfully.";
        const summary = responseBody.preprocessing || {};
        resultOriginalRows.textContent = responseBody.dataset.original_rows;
        resultFinalRows.textContent = responseBody.dataset.final_rows;
        resultMissing.textContent = `${summary.missing_values_before} → ${summary.missing_values_after}`;
        resultDuplicates.textContent = summary.duplicates_found;
        resultEmptyRows.textContent = summary.empty_rows_removed;
        resultInvalid.textContent = summary.invalid_numeric_values + summary.invalid_dates;
        resultCalculated.textContent = summary.sales_values_calculated;
        resultFile.textContent = responseBody.processed_file;
        detectedFields.replaceChildren();
        Object.entries(responseBody.detected_columns || {}).forEach(function addDetectedField(entry) {
          const item = document.createElement("li");
          const confidence = responseBody.field_confidence && responseBody.field_confidence[entry[0]];
          const confidenceLabel = Number.isFinite(Number(confidence)) ? ` (${Math.round(Number(confidence) * 100)}% confidence)` : "";
          item.textContent = `${fieldLabels[entry[0]] || entry[0]}: ${entry[1]}${confidenceLabel}`;
          detectedFields.appendChild(item);
        });
        const profile = responseBody.profile || {};
        profileType.textContent = profile.dataset_type || "Sales dataset";
        profileShape.textContent = `${profile.rows ?? responseBody.dataset.original_rows} rows × ${profile.columns ?? responseBody.dataset.original_columns} columns`;
        profileDateRange.textContent = profile.date_range || "Not available";
        profileForecasting.textContent = profile.forecasting_available ? "Available" : "Unavailable — date and sales fields are both required";
        profileSalesAnalysis.textContent = profile.sales_analysis_available ? "Available" : "Unavailable";
        profileDuplicateIds.textContent = String(profile.duplicate_transaction_ids ?? summary.duplicate_transaction_ids ?? 0);
        renderList(profileDimensions, profile.available_dimensions);
        renderList(profileMissingFields, profile.missing_optional_fields);
        renderList(profileDerivedFields, summary.derived_fields);
        const columnNames = Array.isArray(profile.column_names)
          ? profile.column_names
          : typeof profile.column_names === "string"
            ? [profile.column_names]
            : [];
        profileColumnDetails.replaceChildren();
        columnNames.forEach(function addColumnDetails(name, index) {
          const item = document.createElement("li");
          const type = profile.data_types && profile.data_types[name];
          const missing = profile.missing_by_column && profile.missing_by_column[name];
          const unique = profile.unique_by_column && profile.unique_by_column[name];
          const samples = profile.sample_values && profile.sample_values[name];
          const sampleList = Array.isArray(samples) ? samples.join(", ") : samples || "none";
          item.textContent = `${name} — ${type || "unknown"}; ${missing ?? 0} missing; ${unique ?? "?"} unique; sample: ${sampleList}`;
          profileColumnDetails.appendChild(item);
        });
        if (!profileColumnDetails.childElementCount) {
          const item = document.createElement("li");
          item.textContent = "No column profile available.";
          profileColumnDetails.appendChild(item);
        }
        const warnings = Array.isArray(responseBody.warnings)
          ? responseBody.warnings
          : typeof responseBody.warnings === "string"
            ? [responseBody.warnings]
            : [];
        processingWarning.textContent = warnings.join(" ");
        result.hidden = false;
        setProcessingStep("complete");
      } catch (error) {
        console.error("Dataset upload fetch/network error:", {
          url: apiUrl,
          error: error.message,
          stack: error.stack
        });
        status.textContent = "Unable to connect to the R backend. Please make sure the R API is running.";
      } finally {
        uploadButton.disabled = false;
      }
    });
    ["dragenter", "dragover"].forEach(function addDragState(eventName) {
      dropZone.addEventListener(eventName, function showDragState(event) {
        event.preventDefault();
        dropZone.classList.add("is-dragging");
      });
    });
    ["dragleave", "drop"].forEach(function removeDragState(eventName) {
      dropZone.addEventListener(eventName, function hideDragState(event) {
        event.preventDefault();
        dropZone.classList.remove("is-dragging");
      });
    });
    dropZone.addEventListener("drop", function handleDrop(event) {
      const file = event.dataTransfer.files[0];
      if (file) {
        const transfer = new DataTransfer();
        transfer.items.add(file);
        input.files = transfer.files;
      }
      validateFile(file);
    });
  });
})();
