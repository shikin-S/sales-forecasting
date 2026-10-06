(function initializeDashboard() {
  const apiUrl = "http://localhost:8000/dashboard";
  const charts = {};
  document.addEventListener("DOMContentLoaded", loadDashboard);
  function number(value) { return value === null || value === undefined || value === "Not available" ? "--" : new Intl.NumberFormat("en-IN", {maximumFractionDigits: 2}).format(Number(value)); }
  function currency(value) { return value === null || value === undefined || value === "Not available" ? "--" : `₹${number(value)}`; }
  function set(id, value, formatter) { const element = document.getElementById(id); if (element) element.textContent = formatter ? formatter(value) : (value ?? "--"); }
  function options(horizontal) { return {responsive: true, maintainAspectRatio: false, indexAxis: horizontal ? "y" : "x", plugins: {legend: {display: false}}, scales: {x: {grid: {display: false}}, y: {beginAtZero: true}}}; }
  function chart(id, type, labels, values, horizontal) { const canvas = document.getElementById(id); if (!canvas || !labels?.length || typeof Chart === "undefined") return; if (charts[id]) charts[id].destroy(); charts[id] = new Chart(canvas, {type, data: {labels, datasets: [{data: values, backgroundColor: horizontal ? "#d97745" : "#174a5b", borderColor: "#174a5b", tension: 0.25}]}, options: options(horizontal)}); }
  function toggle(id, emptyId, available) { const canvas = document.getElementById(id); const empty = document.getElementById(emptyId); if (canvas) canvas.hidden = !available; if (empty) empty.hidden = available; }
  function displayCharts(analysis) {
    const time = analysis.time_analysis || {}; const daily = time.daily_sales || []; toggle("dashboard-daily-chart", "dashboard-chart-empty", daily.length > 0); chart("dashboard-daily-chart", "line", daily.map(row => row.date), daily.map(row => row.total_sales), false);
    const monthly = time.monthly_sales || []; toggle("dashboard-monthly-chart", "dashboard-monthly-empty", monthly.length > 0); chart("dashboard-monthly-chart", "bar", monthly.map(row => `${row.month_name} ${row.year}`), monthly.map(row => row.total_sales), false);
    const products = analysis.product_analysis || {}; const top = products.top_products || []; toggle("dashboard-product-chart", "dashboard-product-empty", products.available !== false && top.length > 0); chart("dashboard-product-chart", "bar", top.map(row => row.product), top.map(row => row.total_sales), true);
    const categories = analysis.category_analysis || {}; const categoryRows = categories.category_sales || []; toggle("dashboard-category-chart", "dashboard-category-empty", categories.available !== false && categoryRows.length > 0); chart("dashboard-category-chart", "bar", categoryRows.map(row => row.category), categoryRows.map(row => row.total_sales), false);
    const regions = analysis.regional_analysis || {}; const regionRows = regions.region_sales || []; toggle("dashboard-region-chart", "dashboard-region-empty", regions.available !== false && regionRows.length > 0); chart("dashboard-region-chart", "bar", regionRows.map(row => row.region), regionRows.map(row => row.total_sales), false);
  }
  function displayAdditionalDimensions(dimensions) {
    const chartGrid = document.querySelector(".dashboard-chart-grid");
    if (!chartGrid || !dimensions || typeof dimensions !== "object") return;
    const entries = Object.entries(dimensions).filter(([name, result]) => !["product", "category", "region"].includes(name) && result?.available);
    if (!entries.length) return;
    const section = document.createElement("section");
    section.className = "chart-grid dashboard-chart-grid";
    section.setAttribute("aria-label", "Additional sales dimensions");
    const chartData = [];
    entries.forEach(([name, result]) => {
      const rows = Array.isArray(result.values) ? result.values : result.values && typeof result.values === "object" ? [result.values] : [];
      if (!rows.length) return;
      const article = document.createElement("article");
      article.className = "chart-card";
      const heading = document.createElement("h2");
      heading.textContent = `Sales by ${name.replace(/_/g, " ")}`;
      const wrap = document.createElement("div");
      wrap.className = "analysis-chart-wrap";
      const canvas = document.createElement("canvas");
      canvas.id = `dashboard-adaptive-${name}-chart`;
      canvas.setAttribute("aria-label", `Sales by ${name} chart`);
      wrap.appendChild(canvas);
      article.append(heading, wrap);
      section.appendChild(article);
      chartData.push({id: canvas.id, rows: rows});
    });
    if (section.childElementCount) {
      chartGrid.insertAdjacentElement("afterend", section);
      chartData.forEach(item => chart(item.id, "bar", item.rows.map(row => row.name), item.rows.map(row => row.total_sales), true));
    }
  }
  function displayInsights(insights) { const list = document.getElementById("dashboard-insights"); list.replaceChildren(); Object.values(insights || {}).forEach(text => { const item = document.createElement("li"); item.textContent = text; list.appendChild(item); }); if (!list.children.length) { const item = document.createElement("li"); item.textContent = "No business insights are available yet."; list.appendChild(item); } }
  function displayEvaluation(evaluation) { if (!evaluation?.success) return; const best = evaluation.models.find(model => model.model === evaluation.best_model.name && model.status === "success"); set("dashboard-best-model", `Best model: ${evaluation.best_model.name}`); set("dashboard-mape", best?.mape == null ? "--" : `${number(best.mape)}%`); set("dashboard-rmse", best?.rmse == null ? "--" : currency(best.rmse)); set("dashboard-mae", best?.mae == null ? "--" : currency(best.mae)); }
  async function loadDashboard() {
    const status = document.getElementById("dashboard-status");
    try {
      const response = await fetch(apiUrl); const data = await response.json();
      if (!response.ok || !data.success) throw new Error(data.message || "No sales dataset is available.");
      const analysis = data.analysis; const summary = analysis.summary || {}; const overview = analysis.dataset || {};
      set("dashboard-file", data.dataset.file); set("dashboard-last-processed", data.dataset.file.replace("processed_sales_", "").replace(".csv", "")); set("dashboard-period", overview.date_range); set("dashboard-total-sales", summary.total_sales, currency); set("dashboard-total-quantity", summary.total_quantity, number); set("dashboard-transactions", summary.total_transactions, number); set("dashboard-average-sales", summary.average_sales, currency); set("dashboard-products", overview.unique_products, number); set("dashboard-best-product", analysis.product_analysis?.top_products?.[0]?.product || "Not available"); set("dashboard-trend", analysis.trend?.direction || "Insufficient data"); displayCharts(analysis); displayAdditionalDimensions(analysis.dimension_analysis); displayEvaluation(data.evaluation); displayInsights(data.insights); set("dashboard-forecast-status", data.forecast ? "Forecast available" : "No forecast generated yet."); if (data.forecast) { set("dashboard-forecast-model", data.forecast.model); set("dashboard-forecast-period", `${data.forecast.horizon} observations`); set("dashboard-forecast-total", currency(data.forecast.forecast.reduce((sum, row) => sum + Number(row.predicted_sales), 0))); } status.innerHTML = '<span class="status-dot status-ready"></span>Dashboard ready';
    } catch (error) { console.error("Dashboard request failed:", error); status.innerHTML = '<span class="status-dot"></span>No sales dataset is available'; document.getElementById("dashboard-insights").innerHTML = '<li>No sales dataset is available. Use Upload Dataset to begin.</li>'; }
  }
})();
