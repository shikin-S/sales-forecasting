(function initializeAnalysis() {
  const apiUrl = "http://localhost:8000/analysis";
  const charts = {};
  document.addEventListener("DOMContentLoaded", loadAnalysis);

  function formatNumber(value) { if (value === null || value === undefined || value === "Not available") return "--"; return new Intl.NumberFormat("en-IN", {maximumFractionDigits: 2}).format(Number(value)); }
  function formatCurrency(value) { return value === null || value === undefined || value === "Not available" ? "--" : `₹${formatNumber(value)}`; }
  function setText(id, value, formatter) { const element = document.getElementById(id); if (element) element.textContent = formatter ? formatter(value) : (value ?? "--"); }

  function displayDatasetOverview(dataset) {
    setText("overview-records", dataset.rows, formatNumber); setText("overview-products", dataset.unique_products, formatNumber); setText("overview-categories", dataset.unique_categories, formatNumber); setText("overview-regions", dataset.unique_regions, formatNumber); setText("overview-date-range", dataset.date_range);
  }

  function displaySummary(summary, trend) {
    if (!summary || summary.available === false) { handleAnalysisError(summary?.message || "Sales analysis cannot be performed because a valid sales field was not detected."); return; }
    setText("summary-total-sales", summary.total_sales, formatCurrency); setText("summary-average-sales", summary.average_sales, formatCurrency); setText("summary-minimum-sales", summary.minimum_sales, formatCurrency); setText("summary-maximum-sales", summary.maximum_sales, formatCurrency); setText("summary-median-sales", summary.median_sales, formatCurrency); setText("summary-standard-deviation", summary.standard_deviation, formatCurrency); setText("summary-total-quantity", summary.total_quantity, formatNumber); setText("trend-direction", `Trend: ${trend?.direction || "Insufficient data"}`);
  }

  function chartOptions(horizontal) { return {responsive: true, maintainAspectRatio: false, indexAxis: horizontal ? "y" : "x", plugins: {legend: {display: false}}, scales: {x: {grid: {display: false}}, y: {beginAtZero: true, grid: {color: "#e2eae8"}}}}; }
  function replaceChart(id, type, labels, values, label, horizontal) { const canvas = document.getElementById(id); if (!canvas || !labels?.length || typeof Chart === "undefined") return; if (charts[id]) charts[id].destroy(); charts[id] = new Chart(canvas, {type, data: {labels, datasets: [{label, data: values, backgroundColor: horizontal ? "#d97745" : "#174a5b", borderColor: "#174a5b", borderWidth: 1, tension: 0.25}]}, options: chartOptions(horizontal)}); }
  function showChartOrMessage(chartId, emptyId, available) { const canvas = document.getElementById(chartId); const message = document.getElementById(emptyId); if (canvas) canvas.hidden = !available; if (message) message.hidden = available; }
  function createDailySalesChart(rows) { const available = Array.isArray(rows) && rows.length > 0; showChartOrMessage("daily-sales-chart", "daily-sales-empty", available); if (available) replaceChart("daily-sales-chart", "line", rows.map(row => row.date), rows.map(row => row.total_sales), "Sales", false); }
  function createMonthlySalesChart(rows) { const available = Array.isArray(rows) && rows.length > 0; showChartOrMessage("monthly-sales-chart", "monthly-sales-empty", available); if (available) replaceChart("monthly-sales-chart", "bar", rows.map(row => `${row.month_name} ${row.year}`), rows.map(row => row.total_sales), "Sales", false); }
  function createYearlySalesChart(rows) { const available = Array.isArray(rows) && rows.length > 0; showChartOrMessage("yearly-sales-chart", "yearly-sales-empty", available); if (available) replaceChart("yearly-sales-chart", "bar", rows.map(row => String(row.year)), rows.map(row => row.total_sales), "Sales", false); }
  function createProductChart(analysis) { const rows = analysis?.top_products || []; const available = analysis?.available !== false && rows.length > 0; showChartOrMessage("product-chart", "product-empty", available); if (available) replaceChart("product-chart", "bar", rows.map(row => row.product), rows.map(row => row.total_sales), "Sales", true); }
  function createCategoryChart(analysis) { const rows = analysis?.category_sales || []; const available = analysis?.available !== false && rows.length > 0; showChartOrMessage("category-chart", "category-empty", available); if (available) replaceChart("category-chart", "bar", rows.map(row => row.category), rows.map(row => row.total_sales), "Sales", false); }
  function createRegionChart(analysis) { const rows = analysis?.region_sales || []; const available = analysis?.available !== false && rows.length > 0; showChartOrMessage("region-chart", "region-empty", available); if (available) replaceChart("region-chart", "bar", rows.map(row => row.region), rows.map(row => row.total_sales), "Sales", false); }
  function displayAdaptiveMeasures(analysis) {
    const grid = document.querySelector(".analysis-summary-section .summary-grid");
    if (!grid) return;
    [["Total Profit", analysis.profit_analysis?.total_profit, "profit"], ["Average Discount", analysis.discount_analysis?.average_discount, "discount"]].forEach(function addMeasure(measure) {
      const value = measure[1];
      if (value === null || value === undefined || value === "Not available" || !Number.isFinite(Number(value))) return;
      const card = document.createElement("article");
      card.className = "stat-card";
      const label = document.createElement("span");
      label.textContent = measure[0];
      const metric = document.createElement("strong");
      metric.textContent = measure[2] === "profit" ? formatCurrency(value) : formatNumber(value);
      card.append(label, metric);
      grid.appendChild(card);
    });
  }
  function displayAdditionalDimensions(dimensions) {
    const chartGrid = document.querySelector(".analysis-chart-grid");
    if (!chartGrid || !dimensions || typeof dimensions !== "object") return;
    const entries = Object.entries(dimensions).filter(([name, result]) => !["product", "category", "region"].includes(name) && result?.available);
    if (!entries.length) return;
    const section = document.createElement("section");
    section.className = "chart-grid analysis-chart-grid";
    section.setAttribute("aria-label", "Additional sales dimensions");
    const chartData = [];
    entries.forEach(function addDimension(entry) {
      const name = entry[0];
      const result = entry[1];
      const rows = Array.isArray(result.values) ? result.values : result.values && typeof result.values === "object" ? [result.values] : [];
      if (!rows.length) return;
      const article = document.createElement("article");
      article.className = "chart-card";
      const heading = document.createElement("h2");
      heading.textContent = `Sales by ${name.replace(/_/g, " ")}`;
      const wrap = document.createElement("div");
      wrap.className = "analysis-chart-wrap";
      const canvas = document.createElement("canvas");
      canvas.id = `adaptive-${name}-chart`;
      canvas.setAttribute("aria-label", `Sales by ${name} chart`);
      wrap.appendChild(canvas);
      article.append(heading, wrap);
      section.appendChild(article);
      chartData.push({id: canvas.id, rows: rows});
    });
    if (section.childElementCount) {
      chartGrid.insertAdjacentElement("afterend", section);
      chartData.forEach(item => replaceChart(item.id, "bar", item.rows.map(row => row.name), item.rows.map(row => row.total_sales), "Sales", true));
    }
  }
  function displayTimeHighlights(timeAnalysis) { if (!timeAnalysis?.available) { setText("period-highlights", timeAnalysis?.message || "Insufficient data for this analysis."); return; } setText("period-highlights", `Best month: ${timeAnalysis.best_month} (₹${formatNumber(timeAnalysis.best_month_sales)}) · Highest day: ${timeAnalysis.highest_sales_date} (₹${formatNumber(timeAnalysis.highest_sales_value)})`); }
  function handleAnalysisError(message) { const status = document.getElementById("analysis-status"); const error = document.getElementById("analysis-error"); if (status) status.innerHTML = '<span class="status-dot"></span>Analysis unavailable'; if (error) error.textContent = message; document.querySelectorAll(".chart-empty").forEach(element => { element.hidden = false; }); }

  async function loadAnalysis() {
    try {
      const response = await fetch(apiUrl); const data = await response.json();
      if (!response.ok || !data.success) throw new Error(data.message || "Unable to connect to the R analysis service.");
      displayDatasetOverview(data.dataset); displaySummary(data.summary, data.trend); displayAdaptiveMeasures(data); createDailySalesChart(data.time_analysis.daily_sales); createMonthlySalesChart(data.time_analysis.monthly_sales); createYearlySalesChart(data.time_analysis.yearly_sales); createProductChart(data.product_analysis); createCategoryChart(data.category_analysis); createRegionChart(data.regional_analysis); displayAdditionalDimensions(data.dimension_analysis); displayTimeHighlights(data.time_analysis);
      const status = document.getElementById("analysis-status"); if (status) status.innerHTML = '<span class="status-dot status-ready"></span>Analysis ready'; document.getElementById("analysis-message").textContent = `Processed file: ${data.processed_file}`;
    } catch (error) { console.error("Analysis request failed:", error); handleAnalysisError(error.message.includes("No processed") ? "No processed dataset available. Please upload and process a sales dataset first." : "Unable to connect to the R analysis service."); }
  }
})();
